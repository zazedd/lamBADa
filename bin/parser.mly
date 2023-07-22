%{
  open Ast

  let rec make_apply e = function
    | [] -> failwith "precondition violated"
    | [e'] -> App (e, e')
	  | h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token <int> INT
%token <string> ID
%token PLUS
%token MULT
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token EQ
%token IN
%token IF
%token THEN
%token ELSE
%token FUN
%token ARROW
%token COLON
%token TINT
%token TBOOL
%token EOF

%nonassoc IN
%nonassoc ELSE
%left EQ
%left PLUS
%left MULT

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | e = simple_expr { e }
  | e1 = expr; PLUS; e2 = expr { Bop (Add, e1, e2) }
  | e1 = expr; MULT; e2 = expr { Bop (Mult, e1, e2) }
  | e1 = expr; EQ; e2 = expr { Bop (Eq, e1, e2) }
  | LET; name = ID; COLON; t = typ; EQUALS; binding = expr; IN; body = expr { LetIn (name, t, binding, body) }
  | LET; name = ID; COLON; t = typ; EQUALS; binding = expr; { Let (name, t, binding) }
  | IF; b = expr ; THEN; e1 = expr; ELSE; e2 = expr { If (b, e1, e2) }
  | e = simple_expr; es = simple_expr+ { make_apply e es }
  | FUN; var = ID ; ARROW; e = expr; { AnonFun (var, e) }
  ;

simple_expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN; e = expr; RPAREN { e }
;

typ:
  | TINT { TInt }
  | TBOOL { TBool }
