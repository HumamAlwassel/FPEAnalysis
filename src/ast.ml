(* Parsing information: ((l1,c1),(l2,c2)) represents a symbol
   appearing at line l1 character c1 to line l2 character c2. *) 
type info = (int * int) * (int * int)
    
(* variables *)
type var = string

(* AST of program *)
type ast_prog = input * condition * aexp

(* input variable values *)
and input = (aexp * interval) list

(* conditions on subexpressions of program *)
and condition = (aexp * interval) list

(* intervals *)
and interval = 
| Union of ((float * float) list)

(* arithmetic expressions *)
and aexp =
| Delta of var
| Var of var
| Float of float
| Plus of aexp * aexp
| Minus of aexp * aexp
| Times of aexp * aexp
| Divides of aexp * aexp
| Neg of aexp
| Sqrt of aexp 
| App of func * aexp

(* functions of single input *)
and func = 
| Log
| Exp
| Sin
| Cos
| Tan
| Cot

(* constants *)
let machine_epsilon = epsilon_float
let pi = 4. *. atan 1. 

(* map between expression and its binding value *)
module BindingMap = Map.Make (struct
  type t = aexp
  let compare = Pervasives.compare
end)

(* map between expression and its conditioned value *)
module ConditionMap = Map.Make (struct
  type t = aexp
  let compare = Pervasives.compare
end)

(* compare function between two arithmetic expressions
  return 0 iff the two expressions would produce the same 
  relative error *)
let rec expCompare (exp1 :aexp) (exp2 :aexp) = 
  match (exp1, exp2) with
  | Plus    (e11, e12), Plus    (e21, e22) 
  | Minus   (e11, e12), Minus   (e21, e22)
  | Times   (e11, e12), Times   (e21, e22) 
  | Divides (e11, e12), Divides (e21, e22) -> begin 
    if (expCompare e11 e21 == 0 && expCompare e12 e22 == 0) ||
       (expCompare e11 e22 == 0 && expCompare e12 e21 == 0) 
    then 0 else expCompare e11 e21
  end  
  | Neg e1, Neg e2 
  | Sqrt e1, Sqrt e2 -> expCompare e1 e2
  | App (f1, e1), App (f2, e2) -> begin
    if f1 == f2 then expCompare e1 e2 
    else Pervasives.compare exp1 exp2
  end 
  | _, _ -> Pervasives.compare exp1 exp2

(* map between a expression and its associated delta *)
module Input2DeltaMap = Map.Make (struct
  type t = aexp
  let compare = expCompare
end)

(* map between a expression and its (1+delta)-augmented expression *)
module Input2AugmentedMap = Map.Make (struct
  type t = aexp
  let compare = Pervasives.compare
end)

(* map between an (augmented expression, delta) and the expression's
   derivative with respect to delta *)
module Augmented2DerivMap = Map.Make (struct
  type t = aexp * var
  let compare = Pervasives.compare
end)
