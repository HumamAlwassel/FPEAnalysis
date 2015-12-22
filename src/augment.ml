open Ast 
open Util
open Pprint

let d_index = ref 0

let get_fresh_delta () =
	let i = !d_index in 
	let () = incr d_index in
	"_d" ^ (string_of_int i)

(* see the function augment *)
let rec add_deltas expr in2del_map in2aug_map = 
	let (delta, in2del_map) = 
		try
			(Input2DeltaMap.find expr in2del_map, in2del_map)
		with Not_found -> begin
			let d = 
				match expr with 
				| Delta _ -> failwith "<add_deltas>: this function can not be called on expressions with deltas"
				| Var _ | Float _ | Neg _ -> None 
				| _ -> Some (Plus (Float 1.0, Delta(get_fresh_delta ()))) 
			in 
			(d, Input2DeltaMap.add expr d in2del_map)
		end
	in
	let (res, new_in2del_map, new_in2aug_map) = 
		match expr with
		| Delta _ -> failwith "<add_deltas>: this function can not be called on expressions with deltas"
		| Var _ | Float _ -> (expr, in2del_map, in2aug_map)
		| Plus (e1, e2) -> begin 
			let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			let (e2_res, e2_in2del_map, e2_in2aug_map) = add_deltas e2 e1_in2del_map e1_in2aug_map in 
			(Plus (e1_res, e2_res), e2_in2del_map, e2_in2aug_map)
	  	end
		| Minus (e1, e2) ->  begin 
			let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			let (e2_res, e2_in2del_map, e2_in2aug_map) = add_deltas e2 e1_in2del_map e1_in2aug_map in 
			(Minus (e1_res, e2_res), e2_in2del_map, e2_in2aug_map)
	  	end
		| Times (e1, e2) -> begin 
			let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			let (e2_res, e2_in2del_map, e2_in2aug_map) = add_deltas e2 e1_in2del_map e1_in2aug_map in 
			(Times (e1_res, e2_res), e2_in2del_map, e2_in2aug_map)
	  	end
		| Divides (e1, e2) -> begin 
			let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			let (e2_res, e2_in2del_map, e2_in2aug_map) = add_deltas e2 e1_in2del_map e1_in2aug_map in 
			(Divides (e1_res, e2_res), e2_in2del_map, e2_in2aug_map)
	  	end
		| Sqrt e1 -> begin 
			let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			(Sqrt (e1_res), e1_in2del_map, e1_in2aug_map)
	  	end
	  	| App (f, e1) -> begin 
			let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			(App (f, e1_res), e1_in2del_map, e1_in2aug_map)
	  	end
	  	| Neg e1 -> begin
	  		let (e1_res, e1_in2del_map, e1_in2aug_map) = add_deltas e1 in2del_map in2aug_map in 
			(Neg e1_res, e1_in2del_map, e1_in2aug_map)
	  	end  
	in
	let simple_res = simplify_aexp res in 
	match delta with 
	| Some d -> (Times (simple_res, d), new_in2del_map, Input2AugmentedMap.add expr (Times(simple_res, d)) new_in2aug_map)
	| None -> (simple_res, new_in2del_map, Input2AugmentedMap.add expr simple_res new_in2aug_map)


(** 
	augment an expression and its subexpressions with (1+ delta)'s 
 	
 	return (new_in2del_map, new_in2aug_map, num_deltas), where
	new_in2del_map: maps each subexpression to its associated delta
	new_in2Aug_map: maps each subexpression to its augmented expressions
	deltas: the list of deltas used in augmenting the expression
*)
let augment expr in2del_map in2aug_map = 
	let (_, new_in2del_map, new_in2aug_map) = add_deltas (simplify_aexp expr) in2del_map in2aug_map in 
	let num_deltas = !d_index in 
	let () = d_index:= 0 in 
	let rec helper ind lst = 
		if ind <= 0 then lst else helper (ind-1) (get_fresh_delta()::lst)
	in 
	let deltas = helper num_deltas [] in
	(new_in2del_map, new_in2aug_map, deltas)
