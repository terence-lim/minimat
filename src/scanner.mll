(* Ocamllex scanner for MiniMat *)
(* Minimat by Terence Lim tl2735@columbia.edu for COMS4115 *)
{ open Parser 
  let un_esc s =
        Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let letter = ['a'-'z' 'A'-'Z']
let digit  = ['0'-'9']
let esc    = '\\' ['\\' ''' '"' 'n' 'r' 't']
let esc_ch = ''' (esc) '''
let ascii  = ([' '-'!' '#'-'[' ']'-'~'])
let string = '"' ( (ascii | esc)* as s ) '"'

rule token = parse
  [' ' '\t' '\r' '\n']   { token lexbuf }   (* Whitespace *)
| "/*"                   { comment lexbuf } (* Comments *)
| "float"    { FLOAT }
| "string"   { STRING }
| "sequence" { SEQUENCE }
| "matrix"   { MATRIX }
| "handle"   { HANDLE }
| "external" { EXTERNAL }
| "constant" { CONSTANT }
| "new"      { NEW }
| ':'        { COLON }
| "::"       { COCOLON }
| '['        { LBRACK }
| ']'        { RBRACK }
| '\''       { TRANSPOSE }
| ".*"       { DOTMUL }
| "./"       { DOTDIV }
| ".%"       { DOTREM }
| ".^"       { DOTPOW }
| '('        { LPAREN }
| ')'        { RPAREN }
| '{'        { LBRACE }
| '}'        { RBRACE }
| ';'        { SEMI }
| ','        { COMMA }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| '^'        { POW }
| '%'        { REM }
| '='        { ASSIGN }
| "=="       { EQ }
| "!="       { NEQ }
| '<'        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "&&"       { AND }
| "||"       { OR }
| "!"        { NOT }
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "return"   { RETURN }
| "int"      { INT }
| "bool"     { BOOL }
| "void"     { VOID }
| "true"     { TRUE }
| "false"    { FALSE }
| digit+ as lxm                         { INTLIT(int_of_string lxm) }
| digit*['.']digit+ as lxm              { FLOATLIT(float_of_string lxm) }
| string                                { STRINGLIT(un_esc s) }
| letter (letter | digit | '_')* as lxm { ID(lxm) }
| eof                                   { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
