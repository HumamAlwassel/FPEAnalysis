open Ast

(* linearize a derivative about delta equals zero *)
let rec linearize_deriv expr = 
	match expr with
	| Delta _ -> Float 0.0 
	| Var _ | Float _ -> expr
	| Plus (e1, e2) -> Plus (linearize_deriv e1, linearize_deriv e2) 
	| Minus (e1, e2) -> Minus (linearize_deriv e1, linearize_deriv e2) 
	| Times (e1, e2) -> Times (linearize_deriv e1, linearize_deriv e2)
	| Divides (e1, e2) -> Divides (linearize_deriv e1, linearize_deriv e2)
	| Neg e1 -> Neg (linearize_deriv e1)
	| Sqrt e1 -> Sqrt (linearize_deriv e1)
	| App (f, e1) -> App(f, linearize_deriv e1) 

(* simplify an arithmetic expression *)
let rec simplify_aexp expr = 
	match expr with 	
	| Neg (Neg e1) -> simplify_aexp e1
	| Plus (Neg e1, e2) -> simplify_aexp (Minus (e2, e1)) 
	| Plus (e1, Neg e2) -> simplify_aexp (Minus (e1, e2)) 
	| Minus (Neg e1, e2) -> simplify_aexp (Neg (Plus (e1, e2))) 
	| Minus (e1, Neg e2) -> simplify_aexp (Plus (e1, e2)) 
	| Delta _ | Var _ | Float _ -> expr 
	| Plus (e1, e2) -> begin
		let simple_e1, simple_e2 =(simplify_aexp e1, simplify_aexp e2) in  
		match (simple_e1, simple_e2) with 
		| Float 0.0, _ -> simple_e2
		| _, Float 0.0 -> simple_e1
		| _, _ -> Plus (simple_e1, simple_e2)
	end
	| Minus (e1, e2) -> begin
		let simple_e1, simple_e2 =(simplify_aexp e1, simplify_aexp e2) in  
		match (simple_e1, simple_e2) with 
		| Float 0.0, _ -> simplify_aexp (Neg (simple_e2))
		| _, Float 0.0 -> simple_e1
		| _, _ -> Minus (simple_e1, simple_e2)
	end
	| Times (e1, e2) -> begin
		let simple_e1, simple_e2 =(simplify_aexp e1, simplify_aexp e2) in  
		match (simple_e1, simple_e2) with 
		| Float 0.0, _ 
		| _, Float 0.0 -> Float 0.0
		| Float 1.0, _ -> simple_e2
		| _, Float 1.0 -> simple_e1
		| _, _ -> Times (simple_e1, simple_e2)
	end
	| Divides (e1, e2) -> begin
		let simple_e1, simple_e2 =(simplify_aexp e1, simplify_aexp e2) in  
		match (simple_e1, simple_e2) with 
		| Float 0.0, _ -> Float 0.0
		| _, Float 0.0  -> failwith "Division by Zero"
		| _, Float 1.0 -> simple_e1
		| _, _ -> Divides (simple_e1, simple_e2)
	end
	| Neg e1 -> begin
		match simplify_aexp e1 with 
		| Float 0.0 -> Float 0.0
		| _ as simple_e1 -> Neg simple_e1
	end
	| Sqrt e1 -> begin
		match simplify_aexp e1 with 
		| Float 0.0 -> Float 0.0		
		| _ as simple_e1 -> Sqrt simple_e1
	end
	| App (f, e1) -> 
		App(f, simplify_aexp e1)

(* simplify an interval *)
let rec simplify_interval = function
	| Union lst -> begin 
		let sorted_lst = List.sort (fun t1 t2 -> Pervasives.compare (fst t1) (fst t2)) lst in 
		match sorted_lst with 
		| [] -> failwith "<simplify_interval>: Union of empty interval list"
		| h::t -> Union (simplify_union h t [])
	end 

(* helper function for simplify_interval 
	assumes list of intervals [lst] is sorted by interval start value *)
and simplify_union i lst out = 
	match lst with 
	| [] -> List.rev (i::out)
	| h::t -> begin
		let ((s1, f1), (s2, f2)) = (i, h) in 
		if f1 < s2 then simplify_union h t (i::out)
		else simplify_union (s1, max f1 f2) t out
	end 

(* return the minimum and maximum values in an interval  *)
let interval_min_max = function
	| Union lst -> begin
		let sorted_lst = List.sort (fun t1 t2 -> Pervasives.compare (fst t1) (fst t2)) lst in 
		let simple_interval = 
			match sorted_lst with 
			| [] -> failwith "<interval_min_max>: Union of empty interval list"
			| h::t -> (simplify_union h t [])
		in 
		(fst (List.hd simple_interval), snd (List.hd (List.rev simple_interval)))
	end

(* return the absolute minimum and absolute maximum values in an interval  *)
let interval_abs_min_max = function
	| Union lst -> begin
		let sorted_lst = List.sort (fun t1 t2 -> Pervasives.compare (fst t1) (fst t2)) lst in 
		let simple_interval = 
			match sorted_lst with 
			| [] -> failwith "<interval_min_max>: Union of empty interval list"
			| h::t -> (simplify_union h t [])
		in 
		let (neg_lst, pos_lst) = List.partition (fun v -> (fst v) <= 0.0) simple_interval in
		let abs_min = begin
			match neg_lst, pos_lst with 
			| [], _ -> fst (List.hd pos_lst)
			| _ , [] -> begin
				let least_neg = List.hd (List.rev neg_lst) in 
				if snd least_neg >= 0.0 then 0.0 else abs_float (snd least_neg)
			end 
			| _ , _ -> begin
				let least_neg = List.hd (List.rev neg_lst) in
				let least_pos = List.hd pos_lst in 
				if snd least_neg >= 0.0 then 0.0 else min (snd least_neg) (fst least_pos)
			end
		end
		in
		let abs_max = begin
			match neg_lst, pos_lst with 
			| [], _ -> snd (List.hd (List.rev pos_lst))
			| _ , [] -> abs_float (fst (List.hd neg_lst))
			| _ , _ -> begin
				let most_neg = List.hd neg_lst in
				let most_pos = List.hd (List.rev pos_lst) in 
				max (abs_float (fst most_neg)) (snd most_pos)
			end
		end
		in
		(abs_min, abs_max)
	end
