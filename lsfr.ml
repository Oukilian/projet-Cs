type coef = Zero | Un;;
type lsfr = (coef*coef);;

(**calcul de base sur F2**)
let add_coef = fun m n ->
	match m, n with
	| m, n when m = Zero && n = Zero -> Zero
	| m, n when m = Zero && n = Un -> Un
	| m, n when m = Un && n = Zero -> Un
	| m, n when m = Un && n = Un -> Zero
	| _ -> failwith "impossible value";;



let mult_coef = fun m n ->
	match m, n with
	| c1, c2 when c1 = Zero && c2 = Zero -> Zero
	| c1, c2 when c1 = Zero && c2 = Un -> Zero
	| c1, c2 when c1 = Un && c2 = Zero -> Zero
	| c1, c2 when c1 = Un && c2 = Un -> Un
	| _ -> failwith "impossible value";;

(**pas besoin de dessin je pense xD**)
let nbr_of_elem = function l ->
	let rec aux = fun l1 acc ->
		match l1 with
		| [] -> acc
		| x::ll -> aux ll (acc + 1)
	in aux l 0;; 

(**calcul de la valeur du lsfr**)
let calc_step = function x ->
	let rec aux = fun a acc ->
		match a with
		| [] -> acc
		| (br, valor)::ll -> (aux ll (add_coef acc (mult_coef br valor)))
	in aux x Un;;

(**calculdu lsfr suivant**)
let next_step = function x ->
	let rec aux = fun lsfr new_valor acc ->
		match lsfr with
		| [] -> List.rev(acc)
		| (br, valor)::ll -> (aux ll valor ((br,new_valor)::acc))
	in aux x (calc_step x) [];;

(*calcul en fonction du nbr demander**)
let nieme_valor = fun n lsfr ->
	let rec aux = fun nbr l ->
		match l with
		| [] -> failwith "list vide"
		| (br,r)::ll when nbr = 0 -> r
		| (br,r)::ll -> aux (nbr - 1) ll
	in aux n lsfr;;

let nieme_step = fun n lsfr ->
	let rec aux = fun nbr llsfr acc ->
		match nbr with
		| 0 -> acc
		| nnbr -> aux (nnbr - 1) (next_step llsfr) (calc_step llsfr)
	in aux n (List.rev lsfr) Zero;;

(**calcul de la nieme valeur**)
let nieme = fun n lsfr ->
	match n with
	| n when n < (nbr_of_elem lsfr) -> (nieme_valor n lsfr)
	| n -> (nieme_step (n - (nbr_of_elem lsfr)) lsfr);;

(**petit test*)
let l = [(Un,Zero);(Zero,Un);(Zero,Zero);(Un,Un)];;
next_step l;;
nieme 4 l;;

(**exo 4 question 5**)
let lsfr1 = [(Un,Un);(Zero,Zero);(Zero,Un);(Un,Un);(Zero,Zero);(Zero,Zero);(Un,Un);(Zero,Zero);(Zero,Zero);(Un,Un)];;
let lsfr2 = [(Zero,Zero);(Zero,Zero);(Un,Un)];;
