open Ast

let sp = Printf.sprintf

let rec pprintAexp a = print_string(strAexp a)
and pprintInterval i = print_string(strInterval i)
and pprintBindings lst = print_string (sp "{%s}" (strBindings lst))
and pprintConditions lst = print_string (sp "{%s}" (strConditions lst))
and strAexp = function
  | Delta d -> d
  | Var x -> x
  | Float f -> sp "%g" f
  | Plus (a1, a2) -> sp "(%s + %s)" (strAexp a1) (strAexp a2)
  | Minus (a1, a2) -> sp "(%s - %s)" (strAexp a1) (strAexp a2)
  | Times (a1, a2) -> sp "(%s * %s)" (strAexp a1) (strAexp a2)
  | Divides (a1, a2) -> sp "(%s / %s)" (strAexp a1) (strAexp a2)
  | Neg a -> sp "-(%s)" (strAexp a)
  | Sqrt a -> sp "sqrt(%s)" (strAexp a)
  | App (f, a) -> sp "%s(%s)" (strFunc f) (strAexp a) 
and strFunc = function
  | Log -> "log"
  | Exp -> "exp"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Cot -> "cot"
and strInterval = function 
  | Union lst -> String.concat " U " (List.map (fun (v1,v2) -> sp "[%g, %g]" v1 v2) lst)
and strBindings lst  = 
  String.concat ", " (List.map (fun (k,v) -> sp "%s: %s" (strAexp k) (strInterval v)) lst)
and strConditions lst  = 
  String.concat ", " (List.map (fun (k,v) -> sp "%s: %s" (strAexp k) (strInterval v)) lst)

let pprintMap (keyPprint: ('a -> unit)) (valuePprint: ('b -> unit)) key value () =  
  let () = print_string "{" in
  let () = keyPprint key in
  let () = print_string " -> " in 
  let () = valuePprint value in
  print_string "}\n"

let pprintBindingMap map =
  BindingMap.fold (pprintMap pprintAexp pprintInterval) map () 
and pprintConditionMap map = 
  ConditionMap.fold (pprintMap pprintAexp pprintInterval) map () 
and pprintInput2DeltaMap map = 
  let valuePprint = function
    | None -> print_string "None"
    | Some d -> print_string d
  in
  Input2DeltaMap.fold (pprintMap pprintAexp valuePprint) map ()
and pprintInput2AugmentedMap map = 
  Input2AugmentedMap.fold (pprintMap pprintAexp pprintAexp) map () 
and pprintAugmented2DerivMap map = 
  let keyPprint = 
    (fun (expr, d) -> 
      let () = print_string "(" in 
      let () = pprintAexp expr in 
      print_string (", " ^ d ^ ")"))
  in 
  Augmented2DerivMap.fold (pprintMap keyPprint pprintAexp) map () 
