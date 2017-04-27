(*Exercice 2*)

type cc = (0; 1);;

type coeff = Zero | Un;;
type monof2 = {coef : coeff ; deg : int};;
type polyf2 = monof2 list;;

let rec coef i l = match l with
	| (c, d)::l when d = i -> c
	| (c, d)::l when d != i -> coef i l
	| _ -> Zero;;
 
let degre p = 
	let rec aux p deg = match p with
		| [] -> deg
		| (c, d)::l -> aux l d
  in aux p 0;;

(* SOMME *)
let somme p1 p2 = 
	let rec aux p1 p2 acc k = match k with
		| -1 -> acc
		| _ -> if ((coef k p1) = Un && (coef k p2) = Un || (coef k p1) = Zero  && (coef k p2) = Zero)
							 then (aux p1 p2 acc (k-1)) else (aux p1 p2 ((Un, k)::acc) (k-1))
	in aux p1 p2 [] (max (degre p1) (degre p2));;

let soustraction p1 p2 = somme p1 (multCoeff p2 (Zero));;
																								
let multCoeff p a = if a = Zero then [] else 
	let rec aux p a acc = match p with 
		| (c, d)::l when c = Un -> aux l a ((Un, d)::acc)
		| (c, d)::l when c = Zero -> aux l a ((Zero, d)::acc)
		| _ -> List.rev(acc)
	in aux p a [];;

let multXn p n = 
	let rec aux p n acc = match p with
		| (c, d)::l -> aux l n ((c, d + n)::acc)
		| _ -> List.rev(acc)
	in aux p n [];;

let cut p i =
	let rec aux p i (l1, l2) = match p with
		| (c, d)::l when d < i -> aux l i ((c, d)::l1, l2)
		| (c, d)::l -> aux l i (l1, (c, d-i)::l2)
		| _ -> (List.rev(l1), List.rev(l2))
	in aux p i ([], []);;	

let renv a k =
	let rec aux a k acc = match a with
		| (c, d)::l -> aux l k ((c , k-d)::acc)   
		| [] -> acc 
	in aux a k [];;   

let rec floatExp x = function
	| 0 -> 1.
  | n -> (if n mod 2 = 0 then 1. else x) *. floatExp(x*.x) (n/2);;

let rec eval p x = 
	let rec aux p x acc = match p with
		| [] -> acc
		| (c, d)::l when c = Un -> aux l x (acc +. floatExp x d)
		| (c, d)::l when c = Zero -> aux l x acc
	in aux p x 0.;;

let p = [(Un, 2); (Un, 3); (Zero, 9)];; 

eval p 2.;;

let rec derive = function 
	| [] -> []
	| (c, 0)::l -> derive l
	| (Un, d)::l -> if d mod 2 != 0 then (Un, (d-1))::(derive l) else derive l
	| (Zero, d)::l -> derive l;; 

let rec newton p x = function
	| 0 -> x
	| n -> newton p (x-.(eval p x)/.(eval (derive p) x)) (n-1);;

let division_newton a b =
	if (degre a) < (degre b) then (0, a) else let p = newton (renv b (degre b)) (float_of_int((degre a)) -. float_of_int((degre b)) +. 1.)
	 in let q = renv ((karatsuba (renv a (degre a)) p) mod (Un, (degre a) - (degre b) + 1)) ((degre a) - (degre b))
	 in (q, (soustraction a (karatsuba q b)));;  