(* Abstract Syntax Tree and functions for printing it *)
(* Minimat by Terence Lim tl2735@columbia.edu for COMS4115 *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Pow | Rem | Dotmul | Dotdiv | Dotrem | Dotpow

type uop = Neg | Not | Transpose

type typ = Int | Bool | Void | Float | Handle | String | Sequence | Matrix 

type bind = typ * string

type decltyp = Declexternal | Declfunction | Declconstant

type expr =
    Literal of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Stride of expr * expr * expr
  | Seqselect of expr * expr
  | Seqassign of expr * expr * expr
  | Matselect of expr * expr * expr
  | Matassign of expr * expr * expr * expr
  | Call of string * expr list
  | SeqLit of expr list
  | MatLit of expr list list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
    decltyp : decltyp;  (* local function | external function | constant *)
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Pow -> "^"
  | Rem -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Dotmul -> ".*"
  | Dotdiv -> "./"
  | Dotrem -> ".%"
  | Dotpow -> ".^"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Transpose -> "Tr "

let rec string_of_expr = function
    Literal(l)     -> string_of_int l
  | FloatLit(f)    -> string_of_float f
  | StringLit(s)   -> "\"" ^ s ^ "\""
  | BoolLit(true)  -> "true"
  | BoolLit(false) -> "false"
  | Id(s)          -> s
  | Binop(e1, o, e2)   ->  
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e)         -> string_of_uop o ^ string_of_expr e
  | Assign(v, e)       -> v ^ " = " ^ string_of_expr e
  | Stride(b, s, e) -> 
      string_of_expr b ^ ":" ^ string_of_expr s ^ ":" ^ string_of_expr e
  | Seqselect(v, e)    -> string_of_expr v ^ "[" ^ string_of_expr e ^ "]"
  | Seqassign(v, e, x) -> 
      string_of_expr v ^ "[" ^ string_of_expr e ^ "] = " ^ string_of_expr x 
  | Matselect(v, e1, e2) -> string_of_expr v ^ "[" ^ string_of_expr e1 
      ^ ", " ^ string_of_expr e2 ^ "]"
  | Matassign(v, e1, e2, x) -> string_of_expr v ^ "[" ^ string_of_expr e1 
      ^ ", " ^ string_of_expr e2 ^ "] = " ^ string_of_expr x 
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | SeqLit (el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | MatLit (el) -> "[" ^ String.concat "; " (List.map (fun e2 -> 
      String.concat ", " (List.map string_of_expr e2)) el) ^ ";]"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> 
      "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int      -> "int"
  | Handle   -> "handle"
  | Bool     -> "bool"
  | Void     -> "void"
  | Float    -> "float"
  | String   -> "string"
  | Sequence -> "sequence"
  | Matrix   -> "matrix"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl = match fdecl.decltyp with
| Declexternal ->  "external " ^ string_of_typ fdecl.typ ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ");\n"
| Declfunction -> string_of_typ fdecl.typ ^ " " ^ fdecl.fname ^ 
    "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"
| Declconstant -> match (List.hd fdecl.body) with
    Return(e) -> "constant " ^ (string_of_typ fdecl.typ) ^ " " ^ fdecl.fname 
      ^ " = " ^ (string_of_expr e) ^ ";\n"
  | _ -> ""
	
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
