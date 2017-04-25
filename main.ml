type coef = Zero | Un;;
type monome = coef*int ;;

let add_coef = fun m n ->
	match m, n with
	| m, n when m = Zero && n = Zero -> Zero
	| m, n when m = Zero && n = Un -> Un
	| m, n when m = Zero && n = Un -> Un
	| m, n when m = Un && n = Un -> Zero
	| _ -> failwith "impossible value";;

let add_monome = fun m n->
	match m, n with
	| c1, c2 when c1 = Zero && c2 = Zero -> Zero
	| c1, c2 when c1 = Zero && c2 = Un -> Un
	| c1, c2 when c1 = Zero && c2 = Un -> Un
	| c1, c2 when c1 = Un && c2 = Un -> Zero
	| _ -> failwith "impossible value";;

let mult_coef = fun m n ->
	match m, n with
	| c1, c2 when c1 = Zero && c2 = Zero -> Zero
	| c1, c2 when c1 = Zero && c2 = Un -> Zero
	| c1, c2 when c1 = Zero && c2 = Un -> Zero
	| c1, c2 when c1 = Un && c2 = Un -> Un
	| _ -> failwith "impossible value";;

let add_poly = fun f p ->
	let rec aux = fun l m acc ->
		match l,m with
		| [],[] -> List.rev(acc)
		| [],(x,y)::mm ->aux l mm ((x,y)::acc)
		| (a,b)::ll,[] -> aux ll m ((a,b)::acc)
		| (a,b)::ll, (x,y)::mm when b == y -> if((add_coef a x) == Zero ) then aux ll mm acc else aux ll mm ((Un, b)::acc)
		| (a,b)::ll, (x,y)::mm when b < y -> (aux ll m ((a,b)::acc))
		| (a,b)::ll, (x,y)::mm when b > y -> (aux l mm ((x,y)::acc))
		| _ -> failwith "cas impossible"
	in aux f p [];;

let rec multXn = fun p m ->
	let rec aux = fun p1 m1 acc ->
  	match p1,m1  with
  	| [],_ -> List.rev(acc)
  	| (a,b)::ll,(x,y) -> aux ll m1 (((mult_coef a x),b+y)::acc)
	in aux p m [];;

let deg = function p ->
  	match List.rev(p) with
  	| (a, b)::l -> b
		| [] -> failwith "impossible value"

let deg_pair = function p ->
	match (deg p) with
	| n when n mod 2 != 0 -> n/2 + 1
	| n -> n/2;;

let multCoeff = fun f a ->
	let rec aux = fun p b acc ->
		match p with
		| [] -> List.rev(acc)
		| (x,y)::l -> (aux l b (((mult_coef x b), y)::acc))
	in aux f a [];;

let cut = fun p i ->
	let rec aux = fun f n acc1 acc2 ->
		match f with
		| [] -> List.rev(acc1),List.rev(acc2)
		| (a,b)::ll when b < n -> (aux ll n ((a,b)::acc1) acc2)
		| (a,b)::ll -> (aux ll n acc1 ((a, (b - n))::acc2))
	in aux p i [] [];;

let rec karatsuba = fun p q ->
	match (deg p), (deg q), p, q with
	| n,_,p2::ll,_ when n < 2 -> (multCoeff q (fst p2))
	| _,m,_,q2::ll when m < 2 ->(multCoeff p (fst q2))
	| a,b,_,_ -> let p0 = (fst (cut p a))
						and p1 = (snd (cut p a))
						and q0 = (fst (cut q b))
						and q1 = (snd (cut q b)) in
						let c0 = (karatsuba p0 q0)
						and c2 = (karatsuba p1 q1) in
						let p0plusp1 = (add_poly p0 p1)
						and q0plusq1 = add_poly q0 q1
						and moinsc0 = multCoeff c0 Un
						and moinsc2 = multCoeff c2 Un
						in
						let  c1 = (add_poly
												(add_poly 
													(karatsuba p0plusp1	q0plusq1) 
													moinsc0) 
												moinsc2)
						in (add_poly (add_poly c0 (multXn c1 (Un, (deg_pair p)))) (multXn c2 ((Un, (deg_pair p) * 2))));;

let pol2 = [(Un,0);(Zero,1);(Un,2);(Un,3)];;		 
let pol = [(Un,1);(Un,2);(Un,3)];;

karatsuba pol pol2;;
cut pol2 (deg_pair pol2);;