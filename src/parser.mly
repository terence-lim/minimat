/* Ocamlyacc parser for MiniMat */
/* Minimat by Terence Lim tl2735@columbia.edu for COMS4115 */

%{
open Ast 
(* Uncomment next line to trace all states parsed:
let _ = Parsing.set_trace true 
*)
%}

%token SEQUENCE MATRIX COLON COCOLON LBRACK RBRACK
%token FLOAT TRANSPOSE STRING
%token EXTERNAL HANDLE CONSTANT NEW
%token POW REM DOTDIV DOTMUL DOTREM DOTPOW
%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL VOID
%token <int> INTLIT
%token <string> ID
%token <float> FLOATLIT
%token <string> STRINGLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left COLON COCOLON
%left PLUS MINUS
%left TIMES DIVIDE REM DOTMUL DOTDIV DOTREM
%left POW DOTPOW
%right NOT NEG
%left TRANSPOSE

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
         fname = $2;
         formals = $4;
         locals = List.rev $7;
         body = List.rev $8;
         decltyp = Declfunction } }
|  EXTERNAL typ ID LPAREN formals_opt RPAREN SEMI
     { { typ = $2;
         fname = $3;
         formals = $5;
         locals = [];
         body = [];
         decltyp = Declexternal } } /* external function declaration */
|  CONSTANT typ ID ASSIGN expr SEMI
     { { typ = $2;
         fname =  "%"^$3;
         formals = [];
         locals = [];
         body =  [Return($5)];
         decltyp = Declconstant } } /* global constant, prefix with "%" */

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT      { Int }
  | BOOL     { Bool }
  | HANDLE   { Handle }
  | FLOAT    { Float }
  | STRING   { String }
  | SEQUENCE { Sequence }
  | MATRIX   { Matrix }
  | VOID     { Void }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1 }
  | RETURN SEMI                             { Return Noexpr }
  | RETURN expr SEMI                        { Return $2 }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INTLIT           { Literal($1) }
  | FLOATLIT         { FloatLit($1) }
  | STRINGLIT        { StringLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr POW    expr { Binop($1, Pow,   $3) }
  | expr REM    expr { Binop($1, Rem,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr DOTMUL expr { Binop($1, Dotmul,  $3) }
  | expr DOTDIV expr { Binop($1, Dotdiv,   $3) }
  | expr DOTREM expr { Binop($1, Dotrem,   $3) }
  | expr DOTPOW expr { Binop($1, Dotpow,   $3) }
  | expr TRANSPOSE   { Unop(Transpose,$1) }
  | LBRACK rows SEMI RBRACK                      { MatLit(List.rev $2) }
  | LBRACK actuals_opt RBRACK                    { SeqLit($2) }
  | ID LBRACK expr COMMA expr RBRACK ASSIGN expr { Matassign(Id($1),$3,$5,$8)}
  | ID LBRACK expr COMMA expr RBRACK             { Matselect(Id($1),$3,$5) }
  | ID LBRACK expr RBRACK ASSIGN expr            { Seqassign(Id($1),$3,$6) }
  | ID LBRACK expr RBRACK                        { Seqselect(Id($1),$3) }
  | expr COLON expr COLON expr                   { Stride($1,$3,$5) }
  | expr COCOLON expr                            { Stride($1,Literal(1),$3) }
  | MINUS expr %prec NEG                         { Unop(Neg, $2) }
  | NOT expr                                     { Unop(Not, $2) }
  | ID ASSIGN expr                               { Assign($1, $3) }
  | NEW typ LPAREN actuals_opt RPAREN            { Call(string_of_typ $2, $4)}
  | ID LPAREN actuals_opt RPAREN                 { Call($1, $3) }
  | LPAREN expr RPAREN                           { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
      
rows:
    actuals_opt            { [$1] }
  | rows SEMI actuals_opt  { $3 :: $1 }
