%{
open Ast
open Printf
open Lexing

let merge (fn,pos1,_) (_,_,pos2) = (fn,pos1,pos2)
%}

%token <float> FLOAT
%token <Ast.var> VAR
%token PLUS MINUS TIMES DIVIDES NEG SQRT 
%token LOG EXP SIN COS TAN COT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA COLON
%token EOF

%left PLUS MINUS
%left TIMES DIVIDES
%left NEG	SQRT 
%left LOG EXP SIN COS TAN COT

%type <Ast.ast_prog> prog 
%type <Ast.interval> interval 
%type <Ast.aexp> aexp 

%start prog
 
%%

/* Programs */
prog:
  | i = input; c = condition; p = aexp; EOF	{ (i, c, p) }
  ;

/* input values (var to interval binding) */
input:
  | LBRACE; b = bind_lst; RBRACE  { b }
  ;

/* conditions on subexpressions (expr to interval binding) */
condition:
  | LBRACE; b = bind_lst; RBRACE  { b }
  ;

bind_lst:
  | b = separated_list(COMMA, bind)   { b } 
  ;

bind:
  | k = aexp; COLON; v = interval   { (k, v) }
  ;

interval:
  | v = FLOAT                                                         { Union [(v, v)] }
  | MINUS; v = FLOAT                                                  { Union [(-1.0*.v,-1.0*.v)] }
  | LBRACKET; v1 = FLOAT; COMMA; v2 = FLOAT; RBRACKET                 { Union [(v1, v2)] }
  | LBRACKET; MINUS; v1 = FLOAT; COMMA; v2 = FLOAT; RBRACKET          { Union [(-1.0*.v1, v2)] }
  | LBRACKET; MINUS; v1 = FLOAT; COMMA; MINUS; v2 = FLOAT; RBRACKET   { Union [(-1.0*.v1, -1.0*.v2)] }
  ;

/* Arithmetic Expressions */
aexp:
	| f = FLOAT                    { Float(f) }
	| x = VAR                      { Var(x) }
	| a = aexp; PLUS; b = aexp     { Plus(a, b) }
	| a = aexp; MINUS; b = aexp    { Minus(a, b) }
	| a = aexp; TIMES; b = aexp    { Times(a, b) }
	| a = aexp; DIVIDES; b = aexp  { Divides(a, b) }
	| MINUS; a = aexp %prec NEG    { Neg(a) }
	| SQRT; a = aexp               { Sqrt(a) }
	| LOG; a = aexp                { App(Log, a) } 
	| EXP; a = aexp                { App(Exp, a) } 
	| SIN; a = aexp                { App(Sin, a) } 
	| COS; a = aexp                { App(Cos, a) } 
  | TAN; a = aexp                { App(Tan, a) } 
  | COT; a = aexp                { App(Cot, a) } 
	| LPAREN; a = aexp; RPAREN     { a } 
	;
