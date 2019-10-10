%{
%}
   


(* Names *)
%token <Input.ident> NAME
%token UNDERSCORE

(* Parens and punctuation *)
%token LPAREN RPAREN PERIOD
%token COMMA COLON ARROW DARROW

(* Expressions *)
%token BOOL
%token LAMBDA
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE


(* Toplevel commands *)
%token <string> QUOTED_STRING
%token CHECK
%token LOAD
%token EVAL

(* End of input *)
%token EOF


(* Precedence *)
%right ARROW


%start <Input.toplevel list> file
%start <Input.toplevel> commandline

(* Top level parsing *)

file:
    | f=filecontents EOF	{ f }

filecontents:
	|          { [] }
	| d=topcomp PERIOD ds=filecontents { d :: ds }

topcomp:
    | LOAD fn=QUOTED_STRING  { Input.TopLoad fn }
    | DEFINITION x=var_name COLONEQ e=term { Input.TopDef (x,e) }
    | CHECK e=term  { Input.TopCheck e }
    | EVAL e=term  { Input.TopEval e }


(* Main AST parsing *)

ty:
  | BOOL { Input.Bool }
  | t1=ty ARROW t2=ty { Input.Arrow (t1,t2) }
  | LPAR t=ty RPAR { t }

term:
    | LAMBDA a=lambda_abstraction DARROW e=term { Input.Lambda(a,e) }
    | IF e1=term THEN e2=term ELSE e3=term { Input.If(e1,e2,e3) }
    | e=app_term COLON t=ty  { Input.Ascription (e,t) }

app_term:
  | e1=app_term e2=simple_term { Input.Apply (e1,e2) }

simple_term:
  | LPAREN e=term RPAREN { e }
  | TRUE { Input.True }
  | FALSE { Input.False }
  | x=var_name { Input.Var x }

var_name:
  | NAME { $1 }

lambda_abstraction:
  | xs=nonempty_list(var_name) COLON t=term  { [(xs, Some t)] }
  | xs=nonempty_list(var_name) {[(xs, None)] }
  | lst=nonempty_list(typed_binder)          { List.map (fun (xs, t) -> (xs, Some t)) lst }

typed_binder:
  | LPAREN xs=nonempty_list(var_name) COLON t=ty RPAREN { (xs, t) } 



