(* 
  This code is an adaptation of code written by Professor Nate Foster,
  Cornell University for an assignment from CS4110 in Fall 2014.
*)
  
{
open Parser
open Printf
exception Eof
exception LexingError of string

let lineno = ref 1
let linestart = ref (-1)

let newline lexbuf : unit = 
  linestart := Lexing.lexeme_start lexbuf;
  incr lineno

let info lexbuf = 
  let c1 = Lexing.lexeme_start lexbuf in
  let c2 = Lexing.lexeme_end lexbuf in
  let l = !lineno in
  let c = !linestart + 1 in
    ((l, c1 - c),(l, c2 - c - 1))

let error lexbuf msg =
  let i = info lexbuf in
  let t = Lexing.lexeme lexbuf in   
  let ((l1,c1),(l2,c2)) = i in 
  let s = 
    if l2=l1
    then Printf.sprintf "line %d, characters %d-%d" l1 c1 c2
    else Printf.sprintf "line %d, character %d, to line %d, character %d" l1 c1 l2 c2 in 
  let err = Printf.sprintf "%s: lexing error %s at %s."      
    s
    msg 
    t in 
  raise (LexingError err)   
}

let int = ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let comment = "(*" _* "*)"

rule token = parse
| white   { token lexbuf }
| comment { token lexbuf }
| newline { newline lexbuf; token lexbuf }
| int as i   { FLOAT (float_of_string i) }
| float as f { FLOAT (float_of_string f) }
| "inf" { FLOAT infinity}
| "machine_epsilon" {FLOAT epsilon_float}
| "pi"    {FLOAT (4. *. atan 1.)}
| "log"	  { LOG }
| "exp"	  { EXP }
| "sin"	  { SIN }
| "cos"	  { COS }
| "tan"   { TAN }
| "cot"   { COT }
| "+"     { PLUS }
| "-"     { MINUS }
| "*"     { TIMES }
| "/"     { DIVIDES }
| "sqrt"  { SQRT }
| "("     { LPAREN }
| ")"     { RPAREN }
| "{"     { LBRACE }
| "}"     { RBRACE }
| "["     { LBRACKET }
| "]"     { RBRACKET }
| ","     { COMMA }
| ":"     { COLON }
| id as v { VAR(v) }
| eof     { EOF }
| _ as c  { error lexbuf (String.make 1 c) }
