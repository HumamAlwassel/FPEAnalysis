open Ast
open Util
open Pprint

type arith_op = 
| Plus
| Minus
| Times
| Divides

let append = List.rev_append

(* compute the result of arithmetic operation between the intervals 
	[a_min, a_max] and [b_min, b_max] *)
let rec interval_arith (a_min, a_max) op (b_min, b_max) = 
	match op with 
	| Plus -> [(a_min +. b_min, a_max +. b_max)]
	| Minus -> [(a_min -. b_max, a_max -. b_min)]
	| Times -> begin 
		let res_min = min (min (a_min *. b_min) (a_min *. b_max)) (min (a_max *. b_min) (a_max *. b_max)) in 
		let res_max = max (max (a_min *. b_min) (a_min *. b_max)) (max (a_max *. b_min) (a_max *. b_max)) in 
		[(res_min, res_max)]
	end
	| Divides -> begin
		if not (b_min < 0.0 && b_max > 0.0) then begin 
			let res_min = min (min (a_min /. b_min) (a_min /. b_max)) (min (a_max /. b_min) (a_max /. b_max)) in 
			let res_max = max (max (a_min /. b_min) (a_min /. b_max)) (max (a_max /. b_min) (a_max /. b_max)) in 
			[(res_min, res_max)]
		end
		else (interval_arith (a_min, a_max) Times (neg_infinity, 1.0 /. b_min)) @
			 (interval_arith (a_min, a_max) Times (1.0 /. b_max, infinity)) 
	end


(* evaluate the expression under the given bindings and conditions map *)
let rec eval expr bind_map cond_map = 
	let res = 
		match expr with
		| Delta _ | Var _ -> begin
			try
			BindingMap.find expr bind_map
			with Not_found -> failwith "Unable to evaluate expression. Some variables are not binded."
		end
		| Float f -> Union [(f, f)]
		| Plus (e1, e2) -> begin
			let (Union e1_lst, Union e2_lst) = (eval e1 bind_map cond_map, eval e2 bind_map cond_map) in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Plus b) acc) [] e2_lst
			in 
			simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst))
		end 
		| Minus (e1, e2) -> begin
			let (Union e1_lst, Union e2_lst) = (eval e1 bind_map cond_map, eval e2 bind_map cond_map) in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Minus b) acc) [] e2_lst
			in 
			simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst))
		end 
		| Times (e1, e2) -> begin
			let (Union e1_lst, Union e2_lst) = (eval e1 bind_map cond_map, eval e2 bind_map cond_map) in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Times b) acc) [] e2_lst
			in 
			simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst))
		end 
		| Divides (e1, e2) -> begin
			let (Union e1_lst, Union e2_lst) = (eval e1 bind_map cond_map, eval e2 bind_map cond_map) in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Divides b) acc) [] e2_lst
			in 
			simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst))
		end
		| Neg e1 -> begin
			let Union e1_lst = eval e1 bind_map cond_map in 
			Union (List.map (fun (a_min, a_max) -> (-1.0 *. a_max, -1.0 *. a_min)) e1_lst)
		end
		| Sqrt e1  -> begin
			let Union e1_lst = eval e1 bind_map cond_map in
			let (neg_lst, pos_lst) = List.partition (fun v -> (fst v) < 0.0) e1_lst in 
			let () = if neg_lst <> [] then begin
				let () = print_string "Warnning: the Expression " in 
				let () = pprintAexp e1 in 
				let () = print_string " evaluates to " in 
				let () = pprintInterval (Union e1_lst) in 
				let () = print_string " which has negative values." in 
				print_endline "The system will discard the non-positive argument values when evaluating sqrt. Complex numbers are not supported."
			end 
			in
			Union (List.map (fun (a_min, a_max) -> (sqrt (max a_min 0.0), sqrt (max a_max 0.0))) e1_lst)
		end
		| App (f, e1) -> begin 
			simplify_interval (eval_app f e1 bind_map cond_map)
		end
	in
	try
		match res, ConditionMap.find expr cond_map with 
		| Union res_lst, Union cond_lst -> begin
			(* this expression has a condition. apply it to the computed result *)
			let helper (a_min, a_max) = List.fold_left (fun acc (b_min, b_max) -> 
					let clipped_inter = (max a_min b_min, min a_max b_max) in 
					if fst clipped_inter > snd clipped_inter then acc else clipped_inter::acc
				) [] res_lst	
			in 
			simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] cond_lst))
		end
	with Not_found -> res

(* evaluate functions application f(e1) under the given bindings and conditions maps *)
and eval_app f e1 bind_map cond_map =
	let Union e1_lst = eval e1 bind_map cond_map in 
	match f with 
	| Log -> begin
		let (neg_lst, pos_lst) = List.partition (fun v -> (fst v) <= 0.0) e1_lst in 
		let () = if neg_lst <> [] then begin
			let () = print_string "Warnning: the Expression " in 
			let () = pprintAexp e1 in 
			let () = print_string " evaluates to " in 
			let () = pprintInterval (Union e1_lst) in 
			let () = print_string " which has non-positive values." in 
			print_endline "The system will discard the non-positive argument values when evaluating logarithm"
		end
		in
		Union (List.map (fun (ts,tf) -> (log (max ts machine_epsilon), log (max tf machine_epsilon))) e1_lst)
	end
	| Exp -> Union (List.map (fun (ts,tf) -> (exp ts, exp tf)) e1_lst)
	| Sin -> Union (List.fold_left (fun acc i -> append (eval_trig sin i) acc) [] e1_lst)
	| Cos -> Union (List.fold_left (fun acc i -> append (eval_trig cos i) acc) [] e1_lst)
	| Tan -> eval (Divides (App (Sin, e1), App (Cos, e1))) bind_map cond_map
	| Cot -> eval (Divides (App (Cos, e1), App (Sin, e1))) bind_map cond_map

(* evaluate sine or cosine functions on the interval [a,b]*)
and eval_trig trig_fun (a,b) =
	let _2pi = 2.*.pi in 
	let _pi_2 = pi/.2. in
	let _3pi_2 = 3.*.pi/.2. in
	let reduce_to_0_2pi (a,b) =
		(* reduces the interval [a,b] to an equivalent union of 
			intervals that are in [0, 2*pi] *)
		if b -. a >= 2.*.pi then [(0.,_2pi)]
		else begin
			let (a',b') = (mod_float a _2pi, mod_float b _2pi) in
			if b' < a' then [(0.,a'); (b,_2pi)] else [(a', b')] 
		end 
	in 
	let divide_to_subintervals (a,b) = 
		(* divide an interval in [0,2*pi] into union of intervals that are 
			inside the subintervals [0,pi/2], [pi/2,pi], [pi, 3*pi/2], and 
			[3*pi/2, 2*pi]
		*)
		match a>=_pi_2, a>=pi, a>=_3pi_2, b>=_pi_2, b>=pi, b>=_3pi_2 with
		| _, _, true, _, _, true -> [(a,b)]
		| _, true, _, _, _, true -> [(a,_3pi_2); (_3pi_2,b)]
		| _, true, _, _, true, _ -> [(a,b)]
		| true, _, _, _, _, true -> [(a,pi); (pi,_3pi_2); (_3pi_2,b)]
		| true, _, _, _, true, _ -> [(a,pi); (pi,b)]
		| true, _, _, true, _, _ -> [(a,b)]
		| false, _, _, _, _, true -> [(a,_pi_2); (_pi_2,pi); (pi,_3pi_2); (_3pi_2,b)]
		| false, _, _, _, true, _ -> [(a,_pi_2); (_pi_2,pi); (pi,b)]
		| false, _, _, true, _, _ -> [(a,_pi_2); (_pi_2,b)]
		| false, _, _, false, _, _ -> [(a,b)]
		| _ -> failwith ("<eval_trig>: <divide_to_subintervals>: invalid " ^
				" input interval [" ^ (string_of_float a) ^ ", " 
				^ (string_of_float b) ^ "]")
	in 
	let lst = List.fold_left (fun acc i -> append (divide_to_subintervals i) acc) [] (reduce_to_0_2pi (a,b)) in 
	List.fold_left (fun acc (a,b) -> (min (trig_fun a) (trig_fun b), max (trig_fun a) (trig_fun b))::acc) [] lst

(* return a Input2DeltaMap map in which each subexpression that's a subtraction 
	of two expressions within a factor of 2 is mapped to None *)
let rec populate_in2del_map expr bind_map cond_map in2del_map = 
	let res_new_map = 
		match expr with
		| Delta _ | Var _ -> begin
			try
			(BindingMap.find expr bind_map, in2del_map)
			with Not_found -> failwith "Unable to evaluate expression. Some variables are not binded."
		end
		| Float f -> (Union [(f, f)], in2del_map)
		| Plus (e1, e2) -> begin
			let (Union e1_lst, e1_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in 
			let (Union e2_lst, new_in2del_map) = populate_in2del_map e2 bind_map cond_map e1_in2del_map in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Plus b) acc) [] e2_lst
			in 
			(simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst)), new_in2del_map)
		end 
		| Minus (e1, e2) -> begin
			let (Union e1_lst, e1_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in 
			let (Union e2_lst, e2_in2del_map) = populate_in2del_map e2 bind_map cond_map e1_in2del_map in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Minus b) acc) [] e2_lst
			in 
			let res = simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst)) in 
			let new_in2del_map = begin
				if is_within_a_factor_of_2 (Union e1_lst) (Union e2_lst) 
				then Input2DeltaMap.add expr None e2_in2del_map else e2_in2del_map 
			end
			in
			(res, new_in2del_map)
		end 
		| Times (e1, e2) -> begin
			let (Union e1_lst, e1_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in 
			let (Union e2_lst, new_in2del_map) = populate_in2del_map e2 bind_map cond_map e1_in2del_map in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Times b) acc) [] e2_lst
			in 
			(simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst)), new_in2del_map)
		end 
		| Divides (e1, e2) -> begin
			let (Union e1_lst, e1_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in 
			let (Union e2_lst, new_in2del_map) = populate_in2del_map e2 bind_map cond_map e1_in2del_map in 
			let helper a = 
				List.fold_left (fun acc b -> append (interval_arith a Divides b) acc) [] e2_lst
			in 
			(simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] e1_lst)), new_in2del_map)
		end
		| Neg e1 -> begin
			let (Union e1_lst, new_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in 
			(Union (List.map (fun (a_min, a_max) -> (-1.0 *. a_max, -1.0 *. a_min)) e1_lst), new_in2del_map)
		end
		| Sqrt e1  -> begin
			let (Union e1_lst, new_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in
			let (neg_lst, pos_lst) = List.partition (fun v -> (fst v) < 0.0) e1_lst in 
			if neg_lst <> [] then begin
				let () = print_string "ERROR: the Expression " in 
				let () = pprintAexp e1 in 
				let () = print_string " evaluates to " in 
				let () = pprintInterval (Union e1_lst) in 
				let () = print_string " which has negative values\n" in 
				failwith "<populate_in2del_map>: INVALID INPUT: can not evaluate sqrt on negative input. Complex numbers are not supported."
			end
			else (Union (List.map (fun (a_min, a_max) -> (sqrt a_min, sqrt a_max)) e1_lst), new_in2del_map)
		end
		| App (f, e1) -> begin 
			let (Union e1_lst, new_in2del_map) = populate_in2del_map e1 bind_map cond_map in2del_map in 
			(simplify_interval (eval_app f e1 bind_map ConditionMap.empty), new_in2del_map)
		end
	in
	try
		match res_new_map, ConditionMap.find expr cond_map with 
		| (Union res_lst, new_map), Union cond_lst -> begin
			(* this expression has a condition. apply it to the computed result *)
			let helper (a_min, a_max) = List.fold_left (fun acc (b_min, b_max) -> 
					let clipped_inter = (max a_min b_min, min a_max b_max) in 
					if fst clipped_inter > snd clipped_inter then acc else clipped_inter::acc
				) [] res_lst	
			in 
			(simplify_interval (Union (List.fold_left (fun acc a -> append (helper a) acc) [] cond_lst)), new_map)
		end
	with Not_found -> res_new_map 

and is_within_a_factor_of_2 e1 e2 = 
	match e1, e2 with 
	| Union e1_lst, Union e2_lst -> begin
		let half_e1 = Union (List.fold_left (fun acc b -> append (interval_arith (0.5, 0.5) Times b) acc) [] e1_lst) in 
		let half_e2 = Union (List.fold_left (fun acc b -> append (interval_arith (0.5, 0.5) Times b) acc) [] e2_lst) in 
		let twice_e1 = Union (List.fold_left (fun acc b -> append (interval_arith (2.0, 2.0) Times b) acc) [] e1_lst) in 
		let twice_e2 = Union (List.fold_left (fun acc b -> append (interval_arith (2.0, 2.0) Times b) acc) [] e2_lst) in 
		let (e1_min, e1_max) = interval_min_max e1 in 
		let (e2_min, e2_max) = interval_min_max e2 in 
		let (half_e1_min, _) = interval_min_max half_e1 in 
		let (half_e2_min, _) = interval_min_max half_e2 in 
		let (_, twice_e1_max) = interval_min_max twice_e1 in 
		let (_, twice_e2_max) = interval_min_max twice_e2 in 
		(half_e1_min <= e2_min && e2_max <= twice_e1_max) || (half_e2_min <= e1_min && e1_max <= twice_e2_max)
	end
