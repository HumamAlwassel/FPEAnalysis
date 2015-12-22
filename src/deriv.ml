open Ast
open Util

(* 
	differentiate expression and its subexpressions with respect to (w.r.t) d 

	return (d_expr, new_aug2deriv_map), where
	d_expr: the derivative of expr w.r.t d
	new_aug2deriv_map: maps (subexpressions, d) to (simplified derivatives w.r.t d)
*)
let rec deriv expr d aug2deriv_map = 
	let (res, new_aug2deriv_map) = 
		match expr with 
		| Delta x | Var x -> ((if x = d then Float 1.0 else Float 0.0), aug2deriv_map)
		| Float _ -> (Float 0.0, aug2deriv_map)
		| Plus (e1, e2) -> begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			let (e2_res, e2_aug2deriv_map) = deriv e2 d e1_aug2deriv_map in 
			(Plus (e1_res, e2_res), e2_aug2deriv_map) 
		end
		| Minus (e1, e2) -> begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			let (e2_res, e2_aug2deriv_map) = deriv e2 d e1_aug2deriv_map in 
			(Minus (e1_res, e2_res), e2_aug2deriv_map) 
		end
		| Times (e1, e2) -> begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			let (e2_res, e2_aug2deriv_map) = deriv e2 d e1_aug2deriv_map in 
			(Plus (Times (e1_res, e2), Times (e1, e2_res)), e2_aug2deriv_map) 
		end  
		| Divides (e1, e2) -> begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			let (e2_res, e2_aug2deriv_map) = deriv e2 d e1_aug2deriv_map in 
			(Divides (Minus (Times (e1_res, e2), Times (e1, e2_res)), Times (e2, e2)), e2_aug2deriv_map) 
		end 
		| Neg e1 -> begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			(Neg (e1_res), e1_aug2deriv_map) 
		end 
		| Sqrt e1 ->begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			(Divides (e1_res, Times (Float 2.0, expr)), e1_aug2deriv_map) 
		end
		| App (f, e1) ->begin
			let (e1_res, e1_aug2deriv_map) = deriv e1 d aug2deriv_map in 
			(Times(chain_rule f e1, e1_res), e1_aug2deriv_map) 
		end
	in
	let simple_res = simplify_aexp res in 
	(simple_res, Augmented2DerivMap.add (expr, d) simple_res new_aug2deriv_map)

and chain_rule (f: func) (e: aexp) = 
	match f with 
	| Log -> Divides(Float 1.0, e)
	| Exp -> App(Exp, e)
	| Sin -> App(Cos, e)
	| Cos -> Neg(App(Sin, e))
	| Tan -> Divides(Float 1.0, Times (App(Cos, e), App(Cos, e)))
	| Cot -> Divides(Float (-1.0), Times (App(Sin, e), App(Sin, e)))

(* 
	differentiate the simplified expression and its simplified subexpressions 
	with respect to (w.r.t) d 

	return new_aug2deriv_map, where
	new_aug2deriv_map: maps (simplified subexpressions, d) to (simplified derivatives w.r.t d) 
*)
let differentiate expr d aug2deriv_map = 
	let (_ , new_aug2deriv_map) = deriv (simplify_aexp expr) d aug2deriv_map in 
	new_aug2deriv_map
