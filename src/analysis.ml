open Ast
open Pprint
open Util

(* evaluate the worst relative error of a single expression under the given maps *)
let eval_error expr (in2aug_map, lin_aug2deriv_map, bind_map, cond_map, deltas) verbose= 
	let aug_expr = Input2AugmentedMap.find expr in2aug_map in
	let lin_deriv_deltas = List.map (fun d -> Augmented2DerivMap.find (aug_expr, d) lin_aug2deriv_map) deltas in 
	let eval_derivs = List.map (fun deriv_d -> (Eval.eval deriv_d bind_map cond_map)) lin_deriv_deltas in 
	let exact_val = Eval.eval expr bind_map cond_map in 
	let (exact_val_abs_min, exact_val_abs_max) = interval_abs_min_max exact_val in 
	let abs_err = machine_epsilon *. (List.fold_left (fun acc inter -> acc +. snd (interval_abs_min_max inter)) 0.0 eval_derivs) in
	let rel_err = abs_float (abs_err /. exact_val_abs_min) in 
	let () = if verbose then begin
		let () = print_string "\nError analysis for subexpr: " in 
		let () = pprintAexp expr in 
		let () = print_string "\n   Augmented expr: " in 
		let () = pprintAexp aug_expr in 
		let () = print_string "\n   Exact val = " in
		let () = pprintInterval exact_val in 
		let () = List.fold_left2 (fun () d eval_deriv -> 
					let () = print_string ("\n   Derivative w.r.t " ^ d ^ " = ") in 
						pprintInterval eval_deriv) () deltas eval_derivs in 
		let () = print_endline ("\n   Worst case absolute error = " ^ (string_of_float abs_err)) in
		print_endline ("   Worst case relative error = " ^ (string_of_float rel_err))
	end in
	rel_err

(* 
* perform the full error analysis on the expression and its subexpression under the given maps.
* return the worse relative error of expr and list of its critical subexpressions. 
*)
let rec full_error_analysis expr maps critical_exprs verbose = 
	let critical_tol = 1e-13 in
	match expr with
	| Delta _ | Var _ | Float _ -> (0.0, critical_exprs)
	| Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Divides (e1, e2) -> begin
		let (e1_rel_err, e1_cr_exprs) = full_error_analysis e1 maps critical_exprs verbose in 
		let (e2_rel_err, e2_cr_exprs) = full_error_analysis e2 maps e1_cr_exprs verbose in 
		let rel_err = eval_error expr maps verbose in
		if rel_err > critical_tol && (max e1_rel_err e2_rel_err) < critical_tol 
		then begin
			let () = if verbose then print_string "{{{CRITICAL SUBEXPR}}}: consider rewriting this subexpr\n" in
			(rel_err, expr::e2_cr_exprs)
		end 
		else (rel_err, e2_cr_exprs)
	end
	| Neg e1 | Sqrt e1 | App (_, e1) -> begin
		let (e1_rel_err, e1_cr_exprs) = full_error_analysis e1 maps critical_exprs verbose in 
		let rel_err = eval_error expr maps verbose in
		if rel_err > critical_tol && e1_rel_err < critical_tol 
		then begin
			let () = if verbose then print_string "{{{CRITICAL SUBEXPR}}}: consider rewriting this subexpr\n" in
			(rel_err, expr::e1_cr_exprs)
		end 
		else (rel_err, e1_cr_exprs)
	end

(* display the output of full_error_analysis *)
let analysis_report (rel_err, critical_exprs) = 
	let critical_tol = 1e-13 in
	let () = print_endline "\n************** ANALYSIS REPORT **************" in
	let () = print_string ("Worst case relative error = " ^ (string_of_float rel_err)) in 
	let () = if rel_err > critical_tol then print_string " [HIGH RELATIVE ERROR]\n" else print_endline "" in 
	match critical_exprs with 
	| [] -> print_endline "No critical subexpressions were found\n"
	| _ -> begin
		let num_subexpr = List.length critical_exprs in 
		let () = print_string (string_of_int num_subexpr) in 
		let () = 
			if num_subexpr > 1 
			then print_string " critical subexpressions were found. Consider rewriting the following:\n" 
			else print_string " critical subexpression was found. Consider rewriting the following:\n" 
		in
		let () = List.fold_left (fun () expr -> print_string "  "; pprintAexp expr; print_endline "") () critical_exprs in 
		print_endline ""
	end
