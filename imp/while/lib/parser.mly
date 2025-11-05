%{
open Ast
%}

%token TRUE
%token FALSE
%token<string> VAR
%token<string> CONST
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token MUL
%token ADD
%token SUB
%token EQ
%token LEQ
%token SKIP
%token ASSIGN
%token WHILE
%token DO
%token SEQ
%token EOF

%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL
%left SEQ
%nonassoc DO
%nonassoc ELSE

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | v = VAR { Var v }
  | c = CONST { Const (int_of_string c) }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; ADD; e2=expr { Add(e1,e2) }
  | e1=expr; SUB; e2=expr { Sub(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;

cmd:
  | SKIP { Skip }
  | v = VAR; ASSIGN; e=expr { Assign(v,e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e, c1, c2) }
  | WHILE; b = expr; DO; c = cmd { While(b,c) }
  | LPAREN; c = cmd; RPAREN { c }
;
