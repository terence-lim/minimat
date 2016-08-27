(* Code generation: takes a semantically checked AST and produces LLVM IR *)
(* Minimat by Terence Lim tl2735@columbia.edu for COMS4115 *)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context      = L.global_context () in
  let the_module   = L.create_module context "MiniMat"
  and double_t     = L.double_type context
  and i64_t        = L.i64_type  context
  and i32_t        = L.i32_type  context
  and i8_t         = L.i8_type   context
  and i1_t         = L.i1_type   context
  and void_t       = L.void_type context in

  let i8ptr_t      = L.pointer_type i8_t 
  and i32ptr_t     = L.pointer_type i32_t 
  and string_t     = L.pointer_type i8_t 
  and sequence_t   = L.pointer_type i32_t 
  and matrix_t     = L.pointer_type double_t
  and handle_t     = L.pointer_type i64_t in

  let ltype_of_typ = function
      A.Int       -> i32_t
    | A.Handle    -> handle_t
    | A.Bool      -> i1_t
    | A.Float     -> double_t
    | A.Sequence  -> sequence_t
    | A.Matrix    -> matrix_t
    | A.String    -> string_t
    | A.Void      -> void_t in

(* ---------------------------------------------------------------------
  Declare external functions required in codegen
-----------------------------------------------------------------------*)
  let printf_t = L.var_arg_function_type i32_t [| i8ptr_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let snprintf_t = L.var_arg_function_type i32_t [| i8ptr_t |] in
  let snprintf = L.declare_function "snprintf" snprintf_t the_module in

  let memset_t = L.function_type void_t [| i8ptr_t; i32_t; i32_t|] in
  let memset = L.declare_function "memset" memset_t the_module in

  let memcpy_t = L.function_type i32_t [| i8ptr_t; i8ptr_t; i32_t|] in
  let memcpy = L.declare_function "memcpy" memcpy_t the_module in

(*---------------------------------------------------------------------
  Instructions to get/set size info of matrix or sequence object
---------------------------------------------------------------------*)
  let sizeof_offset = L.const_int i32_t (-1)  (* # bytes for storage *)
  and length_offset = L.const_int i32_t (-2)  (* length of sequence *)
  and cols_offset   = L.const_int i32_t (-3)  (* # columns of matrix *)
  and rows_offset   = L.const_int i32_t (-4)  (* # rows of matrix *)
  and string_sz     = L.const_int i32_t 256   (* max chars in string *)
  and int_sz        = L.const_int i32_t 4
  and double_sz     = L.const_int i32_t 8
  and one_32t       = L.const_int i32_t 1
  and zero_32t      = L.const_int i32_t 0 in

  let getdim from_ptr item the_builder =
    let loc = L.build_bitcast from_ptr i32ptr_t "dim" the_builder in
    let loc = L.build_gep loc [| item |] "dim" the_builder in
    L.build_load loc "dim" the_builder in
  
  let putdim from_ptr item the_val the_builder =
    let loc = L.build_bitcast from_ptr i32ptr_t "dim" the_builder in
    let loc = L.build_gep loc [| item |]  "dim" the_builder in
    L.build_store the_val loc the_builder in

(*---------------------------------------------------------------------
  Instructions to allocate storage for new matrix, sequence or string.
  Prepend matrix and sequence object with 16-byte header containing size info.
-----------------------------------------------------------------------*)
  let head_from_body loc the_builder =
    let charptr = L.build_bitcast loc i8ptr_t "new" the_builder in
    L.build_gep charptr [| (L.const_int i8_t (-16)) |]  "new" the_builder in

  let body_from_head loc the_builder =
    let charptr = L.build_bitcast loc i8ptr_t "new" the_builder in
    L.build_gep charptr [| (L.const_int i8_t (16)) |]  "new" the_builder in

  let select_heap = true and select_stack = false in (* pick heap or stack *)

  (* allocate a block of sz bytes from stack or heap *)
  let build_new stack_or_heap sz the_builder = 
    let ch_ptr = (if stack_or_heap = select_heap then 
      L.build_array_malloc i8_t sz "new" the_builder else
      L.build_array_alloca i8_t sz "new" the_builder) in
    ignore (L.build_call memset [| ch_ptr ; zero_32t ; sz |] "" the_builder);
    ch_ptr in

  (* allocate sz elements, each of len bytes from stack or heap *)  
  let build_vecnew stack_or_heap len sz the_builder =
    let sz = L.build_mul len sz "new" the_builder in
    let alloc_sz = L.build_add sz (L.const_int i32_t 16) "new" the_builder in
    let char_ptr = build_new stack_or_heap alloc_sz the_builder in
    let vec_ptr = body_from_head char_ptr the_builder in
    ignore (putdim vec_ptr sizeof_offset alloc_sz the_builder);
    ignore (putdim vec_ptr length_offset len the_builder);
    ignore (putdim vec_ptr rows_offset zero_32t the_builder);
    ignore (putdim vec_ptr cols_offset zero_32t the_builder);
    L.build_bitcast vec_ptr sequence_t "new" the_builder in

  (* allocate row * col elements of double_sz bytes from stack or heap *)
  let build_matnew stack_or_heap row col the_builder =
    let len = L.build_mul row col "new" the_builder in
    let vec_ptr = build_vecnew stack_or_heap len double_sz the_builder in
    ignore (putdim vec_ptr rows_offset row the_builder);
    ignore (putdim vec_ptr cols_offset col the_builder);
    L.build_bitcast vec_ptr matrix_t "new" the_builder in

(*-------------------------------------------------------------
   To put or get a data item from matrix or sequence
--------------------------------------------------------------*) 
  let build_put from_ptr offset the_val the_builder =
    let loc = L.build_gep from_ptr [| offset |] "put" the_builder in
    L.build_store the_val loc the_builder in

  let build_get from_ptr offset the_builder =
    let loc = L.build_gep from_ptr [| offset |] "get" the_builder in
    L.build_load loc "get" the_builder in

  let build_getrc from_ptr row col the_builder =
    let offset = getdim from_ptr cols_offset the_builder in
    let offset = L.build_mul row offset "get" the_builder in
    let offset = L.build_add col offset "get" the_builder in
    build_get from_ptr offset the_builder in

  let build_putrc from_ptr row col the_val the_builder =
    let offset = getdim from_ptr cols_offset the_builder in
    let offset = L.build_mul row offset "getrc" the_builder in
    let offset = L.build_add col offset "getrc" the_builder in
    build_put from_ptr offset the_val the_builder in

  let build_seq_of_int the_val the_builder =
    let to_ptr = build_vecnew select_stack one_32t int_sz the_builder
    in ignore(build_put to_ptr zero_32t the_val the_builder); 
    to_ptr in

  let build_mat_of_float the_val the_builder =
    let to_ptr = build_matnew select_stack one_32t one_32t the_builder
    in ignore(build_put to_ptr zero_32t the_val the_builder); 
    to_ptr in

(*------------------------------------------------------------------
   Declare each global variable; remember its value in a map
-------------------------------------------------------------------*)
  let null_t = L.define_global "_null" (L.const_stringz context "") the_module
  in let build_const_init = function
    | A.Float     -> L.const_float double_t 0.0
    | A.Sequence  -> L.const_null sequence_t
    | A.Matrix    -> L.const_null matrix_t
    | A.String    -> L.const_bitcast null_t string_t
    | A.Handle    -> L.const_null handle_t
    | _ as t      -> L.const_int (ltype_of_typ t) 0
  in

  let global_vars =
    let global_var m (t, n) =
      let init = build_const_init t
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

(*------------------------------------------------------------------
   Populate lists of local functions, externals declarations, and constants
-------------------------------------------------------------------*)
  let local_functions = 
     List.filter (fun fdecl -> fdecl.A.decltyp = A.Declfunction) functions
  and external_functions = 
     List.filter (fun fdecl -> fdecl.A.decltyp = A.Declexternal) functions
  and constant_functions =
     List.filter (fun fdecl -> fdecl.A.decltyp = A.Declconstant) functions in

  let constant_decls = 
     let constant_decl m fdecl = 
      let name = fdecl.A.fname and 
        e1 = (match (List.hd fdecl.A.body) with 
          A.Return(e) -> e | _ -> A.Noexpr) in
      StringMap.add name e1 m in
     List.fold_left constant_decl StringMap.empty constant_functions in

  let external_decls =
    let external_decl m fdecl = 
      let name = fdecl.A.fname and
          formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) 
                                                 fdecl.A.formals)
      in 
      let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types 
      in
      StringMap.add name (L.declare_function name ftype the_module, fdecl) m
    in
    List.fold_left external_decl StringMap.empty external_functions in

  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname 
      and formal_types = Array.of_list 
          (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals) in
    let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty local_functions in

(*--------------------------------------------------------------------
  Helper instructions to call local or external functions by name
---------------------------------------------------------------------*)
    let select_free = true and select_keep = false in (* to free mem or not *)

    (* if mat/seq/string, copy values to heap or stack, else return value *)
    let build_copy ans heap_or_stack free_or_keep the_builder = 
      let t = L.type_of ans in
      (if (t = matrix_t || t = sequence_t) then
        let siz = getdim ans sizeof_offset the_builder in
        let dst = build_new heap_or_stack siz the_builder
        and src = head_from_body ans the_builder in
        ignore(L.build_call memcpy [| dst; src; siz |] "" the_builder);
        ignore(if free_or_keep = select_free then
          L.build_free src the_builder else src);
        L.build_bitcast (body_from_head dst the_builder) t "cp" the_builder
      else if (t = string_t) then
        let dst = build_new heap_or_stack string_sz the_builder in
        ignore(L.build_call memcpy [| dst; ans; string_sz |] "" the_builder);
        ignore(if free_or_keep = select_free then 
          L.build_free ans the_builder else ans);
        dst
      else ans) in

    (* call a locally-defined function by name *)
    let build_funcall f actuals_array the_builder=
      let (fdef, fdecl) = (try StringMap.find f function_decls with
        Not_found -> raise (Failure("Not Found " ^ f))) in
      let result = (match fdecl.A.typ with A.Void -> "" | _ -> f ^ "_res") in
      let ans = L.build_call fdef actuals_array result the_builder
      (* callee returned mat/seq/string in heap, so copy to stack and free *)
      in build_copy ans select_stack select_free the_builder in

    (* call an externally-declared function by name *)
    let build_external fname actuals_array the_builder=
      let (fdef, fdecl) = (try StringMap.find fname external_decls with
        Not_found -> raise(Failure("Not Found" ^ fname))) in
      let result = (match fdecl.A.typ with A.Void -> "" | _ -> fname ^ "_res")
      in L.build_call fdef actuals_array result the_builder in

(*--------------------------------------------------------------------
  Main "inner loop" to iterate on each function record
---------------------------------------------------------------------*)
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m in

      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store (build_const_init t) local_var builder);
        StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (*--------------------------------------------------------------------
      Construct code for an expression; return its value
    ---------------------------------------------------------------------*)
    let rec expr builder = function
        A.Literal i   -> L.const_int i32_t i
      | A.FloatLit f  -> L.const_float double_t f
      | A.BoolLit b   -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit s -> L.build_global_stringptr s "str" builder
      | A.Noexpr      -> L.const_int i32_t 0
      | A.Id s        ->
          (* Id may be in local vars, global vars, or constants lists *)
          if (StringMap.mem s local_vars || StringMap.mem s global_vars) 
          then L.build_load (lookup s) s builder 
          else expr builder (try StringMap.find ("%"^s) constant_decls
          with Not_found -> raise(Failure("ID Not Found " ^ s)))

      (*-------------------------------------------------------------------
        To construct matrix literal, by folding over rows and columns. 
        Calls vertcat() and horzcat() helper functions in standard library
      -------------------------------------------------------------------*)
      | A.MatLit (act) ->
          let v0 = build_matnew select_stack zero_32t zero_32t builder in
          let catadj leftmat right = 
            let rightmat = (if (L.type_of right) != matrix_t then
              build_mat_of_float right builder else right) in 
            build_funcall "horzcat" [| leftmat; rightmat |] builder in
          let makerow row = 
            let actuals = List.rev (List.map (expr builder) (List.rev row)) in
            List.fold_left catadj v0 actuals in
          let rows = List.rev (List.map makerow (List.rev act)) in
          List.fold_left (fun toprow botrow -> 
            build_funcall "vertcat" [| toprow; botrow |] builder) 
            v0 rows

      (*-------------------------------------------------------------------
        Construct sequence literal, by calling append() to fold over list
      -------------------------------------------------------------------*)
      | A.SeqLit (act) ->
          let v0 = build_vecnew select_stack zero_32t int_sz builder and
              actuals = List.rev (List.map (expr builder) (List.rev act)) in
          List.fold_left (fun v1 v2 -> 
            let v3 = (if (L.type_of v2) != sequence_t 
            then build_seq_of_int v2 builder else v2)
            in build_funcall "append" [| v1; v3 |] builder)
            v0 actuals

      (*-------------------------------------------------------------------
        Construct sequence colon expression, by calling stride() helper func
      -------------------------------------------------------------------*)
      | A.Stride (b, s, e) -> let b1 = expr builder b and
            s1 = expr builder s and e1 = expr builder e in
        build_funcall "stride" [| b1; s1; e1 |] builder

      | A.Binop (e1, op, e2) ->
          let e3 = expr builder e1
          and e4 = expr builder e2 in
          let typ = L.type_of e3 in

          (* operands of sequence type *)
          (if typ = sequence_t then (match op with
            A.Add     -> build_funcall "vadd" [| e3; e4 |] builder  
          | A.Sub     -> build_funcall "vsub" [| e3; e4 |] builder  
          | A.Mult    -> build_funcall "vmul" [| e3; e4 |] builder  
          | A.Div     -> build_funcall "vdiv" [| e3; e4 |] builder  
          | A.Rem     -> build_funcall "vrem" [| e3; e4 |] builder  
          | _ -> raise (Failure ((A.string_of_op op) ^ " not defined for " 
                                 ^ (L.string_of_lltype typ) ^ " in " 
                                 ^ (A.string_of_expr e2)))
                )
          (* operands of matrix type *)
          else if typ = matrix_t then (match op with
            A.Add     -> build_funcall "madd"    [| e3; e4 |] builder  
          | A.Sub     -> build_funcall "msub"    [| e3; e4 |] builder  
          | A.Mult    -> build_funcall "mmul"    [| e3; e4 |] builder  
          | A.Div     -> build_funcall "mdiv"    [| e3; e4 |] builder  
          | A.Rem     -> build_funcall "mrem"    [| e3; e4 |] builder  
          | A.Pow     -> build_funcall "mpow"    [| e3; e4 |] builder  
          | A.Equal   -> build_funcall "meq"     [| e3; e4 |] builder  
          | A.Neq     -> build_funcall "mne"     [| e3; e4 |] builder  
          | A.Less    -> build_funcall "mlt"     [| e3; e4 |] builder  
          | A.Leq     -> build_funcall "mle"     [| e3; e4 |] builder  
          | A.Greater -> build_funcall "mgt"     [| e3; e4 |] builder  
          | A.Geq     -> build_funcall "mge"     [| e3; e4 |] builder  
          | A.Dotmul  -> build_funcall "mdotmul" [| e3; e4 |] builder
          | A.Dotdiv  -> build_funcall "mdotdiv" [| e3; e4 |] builder
          | A.Dotrem  -> build_funcall "mdotrem" [| e3; e4 |] builder
          | A.Dotpow  -> build_funcall "mdotpow" [| e3; e4 |] builder
          | _ -> raise (Failure ((A.string_of_op op) ^ " not defined for " 
                                 ^ (L.string_of_lltype typ) ^ " in " 
                                 ^ (A.string_of_expr e2)))
                )
          (* operands of float type *)
          else if typ = double_t then (match op with
            A.Add     -> L.build_fadd e3 e4 "tmp" builder
          | A.Sub     -> L.build_fsub e3 e4 "tmp" builder
          | A.Mult    -> L.build_fmul e3 e4 "tmp" builder
          | A.Div     -> L.build_fdiv e3 e4 "tmp" builder
          | A.Rem     -> L.build_frem e3 e4 "tmp" builder
          | A.Pow     -> build_external "pow" [| e3; e4 |] builder
          | A.Equal   -> L.build_fcmp L.Fcmp.Ueq e3 e4 "tmp" builder
          | A.Neq     -> L.build_fcmp L.Fcmp.Une e3 e4 "tmp" builder
          | A.Less    -> L.build_fcmp L.Fcmp.Ult e3 e4 "tmp" builder
          | A.Leq     -> L.build_fcmp L.Fcmp.Ule e3 e4 "tmp" builder
          | A.Greater -> L.build_fcmp L.Fcmp.Ugt e3 e4 "tmp" builder
          | A.Geq     -> L.build_fcmp L.Fcmp.Uge e3 e4 "tmp" builder
          | _ -> raise (Failure ((A.string_of_op op) ^ " not defined for " 
                                 ^ (L.string_of_lltype typ) ^ " in " 
                                 ^ (A.string_of_expr e2)))
                )
          (* operands of string type *)
          else if typ = string_t then (match op with
          | A.Equal   -> build_funcall "stringeq" [| e3; e4 |] builder  
          | A.Neq     -> build_funcall "stringne" [| e3; e4 |] builder  
          | A.Less    -> build_funcall "stringlt" [| e3; e4 |] builder  
          | A.Leq     -> build_funcall "stringle" [| e3; e4 |] builder  
          | A.Greater -> build_funcall "stringgt" [| e3; e4 |] builder  
          | A.Geq     -> build_funcall "stringge" [| e3; e4 |] builder
          | _ -> raise (Failure ((A.string_of_op op) ^ " not defined for " 
                                 ^ (L.string_of_lltype typ) ^ " in " 
                                 ^ (A.string_of_expr e2)))
                )
          else (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Rem     -> L.build_srem
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure ((A.string_of_op op) ^ " not defined for " 
                                 ^ (L.string_of_lltype typ) ^ " in " 
                                 ^ (A.string_of_expr e2)))
                ) e3 e4 "tmp" builder
              )
      | A.Unop(op, e) ->
          let e' = expr builder e in
          (match op with
          | A.Neg       -> let t = L.type_of e' in
            if (t = double_t) then L.build_fneg e' "tmp" builder
            else if (t = sequence_t) then build_funcall "vneg" [|e'|] builder
            else if (t = matrix_t) then build_funcall "mneg" [|e'|] builder
            else L.build_neg e'"tmp" builder
          | A.Transpose -> build_funcall "mtransp" [| e' |] builder
          | A.Not       -> 
              (if (L.type_of e') = i1_t then L.build_not e' "tmp" builder 
              else build_funcall "vnot" [| e' |] builder))

      (*-------------------------------------------------------------------
         When assigning from matrix/sequence/string r-value, copy its values
         (to stack, but to heap when assigning to global variable identifier)
      -------------------------------------------------------------------*)
      | A.Assign (lv, rv) -> let rv1 = expr builder rv in
        let rv2 = (match rv with  
          (* when r-value is identifier of mat/seq/str, then copy values *)
          A.Id(_) -> build_copy rv1 select_stack select_keep builder
        | _ -> rv1) in let rv3 = (if (StringMap.mem lv local_vars) then rv2
            (* if l-value is global id, then make a copy to heap *)
        else build_copy rv2 select_heap select_keep builder)
        in ignore (L.build_store rv3 (lookup lv) builder); rv3
          
      (*-------------------------------------------------------------------
         Subselect from matrix or sequence with multiple index positions.
         Requires mselect() and vselect() helper functions
       -------------------------------------------------------------------*)
      | A.Matselect (s, r, c) -> let r1 = expr builder r and
            c1 = expr builder c and s1 = expr builder s in
        if (L.type_of r1 = i32_t && L.type_of c1 = i32_t) 
        then (ignore(build_funcall "checkmatrc" [| s1; r1; c1|] builder);
              let v1 = build_getrc s1 r1 c1 builder in
              build_mat_of_float v1 builder)
        else let r2 = (if (L.type_of r1) != sequence_t then 
          build_seq_of_int r1 builder else r1) and
            c2 = (if (L.type_of c1) != sequence_t then
              build_seq_of_int c1 builder else c1)
        in build_funcall "mselect" [| s1; r2; c2 |] builder;
          
      | A.Seqselect (s, e) -> 
          let e1 = expr builder e and s1 = expr builder s in
          if (L.type_of e1) = i32_t
          then (ignore(build_funcall "checkseqlength" [| s1; e1 |] builder);
                let v1 = build_get s1 e1 builder in
                build_seq_of_int v1 builder)
          else build_funcall "vselect" [| s1; e1 |] builder
            
      (*-------------------------------------------------------------------
         Assign to multiple positions in a matrix or sequence
         Requires massign() and vassign() helper functions in standard lib
      -------------------------------------------------------------------*)
      | A.Matassign (s, r, c, v) -> let r1 = expr builder r and c1 = 
          expr builder c and s1 = expr builder s and v1 = expr builder v in
        if (L.type_of r1 = i32_t && L.type_of c1 = i32_t) 
        (* directly put when index r and c are ints *)
        then (ignore(build_funcall "checkmatrc" [| s1; r1; c1|] builder);
              ignore(build_funcall "checkmatscalar" [| v1 |] builder);
              let v2 = build_get v1 zero_32t builder
              in ignore(build_putrc s1 r1 c1 v2 builder); v1)
        else let r2 = (if (L.type_of r1) != sequence_t then
          build_seq_of_int r1 builder else r1)
        and c2 = (if (L.type_of c1) != sequence_t then
          build_seq_of_int c1 builder else c1)
        in build_funcall "massign" [| s1 ; r2 ; c2; v1 |] builder

      | A.Seqassign (s, e, v) -> let e1 = expr builder e and
            s1 = expr builder s and v1 = expr builder v in
          if (L.type_of e1) = i32_t  (* directly put if index e is int *)
          then (ignore(build_funcall "checkseqlength" [| s1; e1 |] builder);
                ignore(build_funcall "checkseqscalar" [| v1 |] builder);
                let v2 = build_get v1 zero_32t builder
                in ignore(build_put s1 e1 v2 builder); v1)
          else build_funcall "vassign" [| s1 ; e1 ; v1 |] builder

      (*-------------------------------------------------------------------
        Type conversion operators
      --------------------------------------------------------------------*)
      | A.Call ("float_of_int", [e]) -> 
          L.build_sitofp (expr builder e) double_t "float_of" builder
      | A.Call ("int_of_float", [e]) -> 
          L.build_fptosi (expr builder e) i32_t "int_of" builder
      | A.Call ("int_of_seq", [e]) -> let e1 = (expr builder e) in
        ignore(build_funcall "checkseqscalar" [| e1 |] builder);
        build_get e1 zero_32t builder
      | A.Call ("float_of_mat", [e]) -> let e1 = (expr builder e) in
        ignore(build_funcall "checkmatscalar" [| e1 |] builder);
        build_get e1 zero_32t builder

     (*----------------------------------------------------------------------
       Construct new matrix, sequence, string allocated from stack
     ----------------------------------------------------------------------*)
      | A.Call ("matrix", [e; e1])  -> 
          build_matnew select_stack (expr builder e) (expr builder e1) builder
      | A.Call ("sequence", [e]) -> 
          build_vecnew select_stack (expr builder e) int_sz builder
      | A.Call ("string", []) -> 
          build_new select_stack string_sz builder
      | A.Call ("string", act) -> 
          let actuals = Array.of_list (List.map (expr builder) act)
          and s = build_new select_stack string_sz builder in
          ignore (L.build_call snprintf
            (Array.append [| s; string_sz |] actuals) "snpr" builder); 
          s
      (*-------------------------------------------------------------------
        Rudimentary output functions, compatible with MicroC
      -------------------------------------------------------------------*)
      | A.Call ("printf", act) -> 
          let actuals = List.map (expr builder) act in
          L.build_call printf_func (Array.of_list actuals) "printf" builder
      | A.Call ("print", [e]) ->
          ignore(build_funcall "printint" [| (expr builder e) |] builder);
          build_funcall "println" [| |] builder
      | A.Call ("printb", [e]) ->
          ignore(build_funcall "printbool" [| (expr builder e) |] builder);
          build_funcall "println" [| |] builder

     (*----------------------------------------------------------------------
       Builtin operators length() cols() return size of matrix or sequence
     ----------------------------------------------------------------------*)
      | A.Call ("length", [e])  ->
          let null = L.build_is_null (expr builder e) "tmp" builder in
          L.build_select null zero_32t  (* return 0 for null objects *) 
            (getdim (expr builder e) length_offset builder) "tmp" builder

      | A.Call ("cols", [e])  ->
          let null = L.build_is_null (expr builder e) "tmp" builder in
          L.build_select null zero_32t  (* return 0 for null objects *) 
            (getdim (expr builder e) cols_offset builder) "tmp" builder

      | A.Call (f, act) ->
          let actuals = List.rev (List.map (expr builder) (List.rev act)) in
          if (StringMap.mem f external_decls) then 
            build_external f (Array.of_list actuals) builder else
            build_funcall f (Array.of_list actuals) builder
    in
    
    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (f builder) in
        
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder

      | A.Return e -> ignore (match fdecl.A.typ with
        (* when return type is mat/seq/str, copy to heap for return *)
        | A.Matrix | A.Sequence | A.String ->
            let e2 = build_copy (expr builder e) select_heap false builder in
            L.build_ret e2 builder
        | A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder e) builder); builder

      | A.If (predicate, then_stmt, else_stmt) ->
          let bool_val = expr builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          
          let then_bb = L.append_block context "then" the_function in
          add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
            (L.build_br merge_bb);
          
          let else_bb = L.append_block context "else" the_function in
          add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
            (L.build_br merge_bb);
          
          ignore (L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
            
      | A.While (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore (L.build_br pred_bb builder);
          
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (stmt (L.builder_at_end context body_bb) body)
            (L.build_br pred_bb);
          
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr pred_builder predicate in
          
          let merge_bb = L.append_block context "merge" the_function in
          ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
            
      | A.For (e1, e2, e3, body) -> stmt builder
          ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in
    
    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in
    
    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
      A.Void -> L.build_ret_void
    | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  
  List.iter build_function_body local_functions;
  the_module
