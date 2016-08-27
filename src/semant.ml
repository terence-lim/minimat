(* Semantic checking for the MiniMat compiler *)
(* Minimat by Terence Lim tl2735@columbia.edu for COMS4115 *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in
  (*----------------------------------------------------------------
     Raise an exception of the given expression is not of the given type 
  ----------------------------------------------------------------*)
  let check_type e t typ_list =
    if not (List.mem t typ_list) then raise (Failure ("illegal type " 
                        ^ string_of_typ t ^ " of " ^ string_of_expr e))
  in
  (**** Checking Global Variables ****)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Global Constants ****)
  report_duplicate (fun n -> "duplicate constant " ^ n)
    (List.map (fun fd -> String.sub fd.fname 1 (String.length fd.fname - 1))
       (List.filter (fun s -> s.decltyp = Declconstant) functions));

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)

  let built_in_decls = 
    List.fold_left (fun m (fdname, fdtyp, fdforms) -> StringMap.add fdname 
                     {typ = fdtyp; fname = fdname; formals = fdforms; 
                      locals = []; body = []; decltyp = Declfunction}
                     m)
                   StringMap.empty
    [("print", Void, [(Int, "x")]);
     ("float_of_int", Float, [(Int, "x")]);
     ("int_of_float", Int, [(Float, "x")]);
     ("int_of_seq", Int, [(Sequence, "x")]);
     ("float_of_mat", Float, [(Matrix, "x")]);
     ("cols", Int, [(Matrix, "x")]);
     ("matrix", Matrix, [(Int, "x"); (Int, "y")]);
     ("sequence", Sequence, [(Int, "y")]);
     ("printb", Void, [(Bool, "x")]);]
  in
  let function_decls = List.fold_left (fun m fd -> 
    StringMap.add fd.fname fd m) built_in_decls functions
  in
  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map snd func.locals);

    (* Type of each variable (global, formal, or local *)
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Global constants are function decls with decltyp Declconstant *)
    let type_of_constant s =
      let fd = function_decl s in match fd.decltyp with Declconstant -> fd.typ
      | _ -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
        Literal _   -> Int
      | BoolLit _   -> Bool
      | FloatLit _  -> Float
      | StringLit _ -> String
      | Id s -> if (func.decltyp = Declconstant) then 
          (* global constant definitions cannot comprise other identifiers *)
          raise (Failure ("constant " ^ (String.sub func.fname 1 
            (String.length func.fname - 1)) ^ 
            " cannot be defined with an identifier " ^ s));
          if (StringMap.mem s symbols) then type_of_identifier s
          (* Global constant identifiers are internally prefixed by "%" *)
          else if (StringMap.mem ("%"^s) function_decls) 
          then type_of_constant ("%"^s)
          else raise (Failure ("undeclared identifier " ^ s))

      (*-----------------------------------------------------
         Checks inside matrix, which is a list of list of floats or matrices
      -----------------------------------------------------*)
      | MatLit(e) -> List.iter (fun e2 -> List.iter (fun e1 -> 
          check_type e1 (expr e1) [Float; Matrix]) e2) e;
          Matrix
            
      (*-----------------------------------------------------
         Checks inside sequence, which is a list of ints or sequences
      -----------------------------------------------------*)
      | SeqLit(e) -> 
          List.iter (fun i -> check_type i (expr i) [Int; Sequence]) e;
          Sequence

      (*-----------------------------------------------------
         checks sequence colon expression b:s:e, which must be ints
      -----------------------------------------------------*)
      | Stride(b, s, e) as ex -> check_type ex (expr b) [Int];
          check_type ex (expr s) [Int];
          check_type ex (expr e) [Int];
          Sequence
            
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
        (match op with
          Add | Sub | Mult | Div | Rem when (t1 = t2) &&
            (t1 = Int || t1 = Float || t1 = Sequence || t1 = Matrix) -> t1
        | Pow when (t1 = Float && t2 = Float) || 
            (t1 = Matrix && t2 = Int) -> t1
        | Dotmul | Dotdiv | Dotrem | Dotpow 
            when (t1 = t2) && t1 = Matrix -> t1
        | Less | Leq | Greater | Geq | Equal | Neq when t1 = t2 && 
            t1 != Sequence -> if t1 = Matrix then Sequence else Bool
        | And | Or when (t1 = t2) && t1 = Bool -> t1
        | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Unop(op, e) as ex -> let t = expr e in
         (match op with
         | Neg when (t = Int || t = Sequence || t = Float || t = Matrix) -> t
         | Not when t = Bool || t = Sequence -> Bool
         | Transpose when t = Matrix -> t    (* MATRIX *)
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op 
                           ^ string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt
                                     ^ " = " ^ string_of_typ rt ^ " in " 
                                     ^ string_of_expr ex))

      (*-----------------------------------------------------
         Check matrix assignment by [row,column] statement: A[i,j] = rhs
         Allow rhs to be a float or matrix.
         Col and row indexes i,j can be int or sequence
      -----------------------------------------------------*)
      | Matassign(var, e1, e2, v) as ex -> check_type ex (expr var) [Matrix];
          check_type ex (expr e1) [Int; Sequence];
          check_type ex (expr e2) [Int; Sequence];
          check_type ex (expr v)  [Matrix];
          Matrix

      (*-----------------------------------------------------
         Check matrix subselect statement: A[i,j]
         Col and row indexes i,j can be int or sequence
      -----------------------------------------------------*)
      | Matselect(var, e1, e2) as ex -> check_type ex (expr var) [Matrix];
          check_type ex (expr e1) [Int; Sequence];
          check_type ex (expr e2) [Int; Sequence];
          Matrix

      (*-----------------------------------------------------
         Check sequence assignment statement: A[i] = rhs
         Index i can be int or sequence
       -----------------------------------------------------*)
      | Seqassign(var, e, v) as ex -> 
          check_type ex (expr var) [Sequence];
          check_type ex (expr e) [Int; Sequence];
          check_type ex (expr v) [Sequence]; 
          Sequence

      (*-----------------------------------------------------
         Checks sequence subselect statement: V[i]
         Index i can be int or sequence
      -----------------------------------------------------*)
      | Seqselect(var, e) as ex -> 
          check_type ex (expr var) [Sequence];
          check_type ex (expr e) [Int; Sequence];
          Sequence

      | Call("printf", _)            -> Void
      | Call("string", _)            -> String
      | Call("length", [e]) as ex    -> 
          ignore(check_type ex (expr e) [Sequence; Matrix]); Int
      | Call(fname, actuals) as call -> let fd = function_decl fname in
        if List.length actuals != List.length fd.formals then
          raise (Failure ("expecting " ^ string_of_int (List.length 
             fd.formals) ^ " arguments in " ^ string_of_expr call))
        else
          List.iter2 (fun (ft, _) e -> let et = expr e in
          ignore (check_assign ft et 
                    (Failure ("illegal actual argument found " ^ 
                              string_of_typ et ^ " expected " ^ 
                              string_of_typ ft ^ " in " ^ string_of_expr e))))
            fd.formals actuals;
        fd.typ
    in

    let check_bool_expr e = if expr e != Bool then
      raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    else () in
    
    (* Verify a statement or throw an exception *)
    let rec stmt = function
        Block sl -> let rec check_block = function
            [Return _ as s] -> stmt s
          | Return _ :: _ -> raise (Failure "nothing may follow a return")
          | Block sl :: ss -> check_block (sl @ ss)
          | s :: ss -> stmt s ; check_block ss
          | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
        raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                        string_of_typ func.typ ^ " in " ^ string_of_expr e))
          
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
          ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in 
    stmt (Block func.body)
  in
  List.iter check_function functions
