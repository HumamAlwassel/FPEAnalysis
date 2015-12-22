open Ast
open Util
open Pprint

let () =
  (* ************** parse input file ************ *)
  let _ = 
    if not (Array.length Sys.argv = 2 || 
      (Array.length Sys.argv = 3 && Sys.argv.(1) = "-v")) 
    then (print_endline ("Usage: " ^ Sys.argv.(0) ^ " [-v] <filename>\n"); exit 0) in 
  let verbose = Array.length Sys.argv = 3 in
  let filename = Sys.argv.((Array.length Sys.argv) - 1) in 
  let lexbuf = Lexing.from_channel (open_in filename) in 
	let (bindings, conditions, prog) = begin 
    try
    Parser.prog Lexer.token lexbuf
    with Parsing.Parse_error ->
      Printf.printf "Syntax error at line %d character %d\n" 
        !Lexer.lineno 
        (Lexing.lexeme_end lexbuf - !Lexer.linestart - 1);
      exit 1
  end
  in
  let prog = simplify_aexp prog in 
  (* ************** display input ************ *)
  let () = print_string "\nVariables bindings: " in 
  let () = pprintBindings bindings in 
  let () = print_string "\nConditions: " in 
  let () = pprintConditions conditions in 
  let () = print_string "\nProgram Expression: " in 
  let () = pprintAexp prog in 
  let () = print_endline "" 
  in 
  (* ************** compute augmented expressions and populate maps ************ *)
  let bind_map = List.fold_left (fun acc (k,v) -> BindingMap.add (simplify_aexp k) (simplify_interval v) acc) BindingMap.empty bindings in
  let cond_map = List.fold_left (fun acc (k,v) -> ConditionMap.add (simplify_aexp k) (simplify_interval v) acc) ConditionMap.empty conditions in 
  let initial_in2del_map = snd (Eval.populate_in2del_map prog bind_map cond_map Input2DeltaMap.empty) in
  let (_, in2aug_map, deltas) = Augment.augment prog initial_in2del_map Input2AugmentedMap.empty in
  let prog_aug = Input2AugmentedMap.find prog in2aug_map in 
  let aug2deriv_map = List.fold_left (fun map d -> Deriv.differentiate prog_aug d map) Augmented2DerivMap.empty deltas in 
  let lin_aug2deriv_map = Augmented2DerivMap.map (fun v -> simplify_aexp (linearize_deriv v)) aug2deriv_map 
  in
  (************ perform the error analysis *************)
	let result = Analysis.full_error_analysis prog (in2aug_map, lin_aug2deriv_map, bind_map, cond_map, deltas) [] verbose in
  Analysis.analysis_report result
