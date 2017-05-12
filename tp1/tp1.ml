type element = int;;
type couleur = Rouge | Noir | DoubleNoir;;
type ab = Vide | VideNoir | Noeud of ab*element*couleur*ab;;

type element_augmente = Min | Element of element | Max;;

let compar =fun x y ->
	match (x,y) with
	| (Min,Min) -> false
	| (Min, _) -> true
	| ((Element e), (Element f)) -> (e < f)
	| (_, Max)	-> true
	| _ -> false;;

let rec appartient_a = fun e a ->
	match a with
	| Vide -> false
	| Noeud(_,e2,_,_) when e2 = e -> true
	| Noeud(_,e2,_,abd) when e2 < e -> appartient_a e abd
	| Noeud(abg,e2,_,_) -> appartient_a e abg
	| _ -> failwith "impossible value";;

let est_vide = function
	| Vide ->true
	| _ -> false;;

let est_abr = function ab ->
	let rec aux = fun a min max ->
		match a with
		| Vide -> true
		| Noeud(_,e,_,_) when ((e >= max) || ( e <= min)) -> false
		| Noeud(ag,e,_,ad) -> ((aux ag min e) && (aux ad e max))
		| _ -> failwith "impossible case"
	in aux ab min_int max_int ;;

let est_abrprof = function ab ->
	let rec aux = function l ->
		match l with
		| [] -> true
		| (Vide,min,max) ::ll -> aux ll
		| (Noeud(ag,e,_,ad),min,max)::ll when (compar min (Element e)) && (compar (Element e) max) -> (aux ((ag,min,(Element e))::(ad,(Element e),max)::ll))
		| _ -> false
	in (aux [(ab,Min,Max)]);;

let racine_noir = function ab ->
	match ab with
	| Vide -> true
	| Noeud(_,_,Rouge,_) -> false
	| Noeud(_,_,Noir,_) -> true
	| _ -> failwith "impossible value";;


let noeud_rouge = function ab ->
	let rec aux = function l ->
		match l with
		| [] -> true
		| (Vide,_)::ll -> aux ll
		| (Noeud(ag,_,Noir,ad),_)::ll -> (aux ((ag,Noir)::(ad,Noir)::ll))
		| (Noeud(ag,_,Rouge,ad),Noir)::ll -> (aux ((ag,Rouge)::(ad,Rouge)::ll))
		| _ -> false
	in aux [(ab,Noir)];;

let nb_noeud_noir = function ab ->
	let rec calcul_noir = fun ab acc ->
		match ab with
		| Vide -> acc
		| Noeud(ag,_,Rouge,_) -> calcul_noir ag acc
		| Noeud(ag,_,Noir,_) -> calcul_noir ag (acc + 1)
		| Noeud(ag,_,DoubleNoir,_) -> calcul_noir ag (acc + 2)
		| _ -> failwith "impossible value"
	and parcours_arbre = fun l n ->
		match l with
		| [] -> true
		| (Vide,x)::ll when (x = n) -> (parcours_arbre ll n)
		| (Noeud(ag,_,Noir,ad),x)::ll -> (parcours_arbre ((ag,x+1)::(ad,x+1)::ll) n)
		| (Noeud(ag,_,Rouge,ad),x)::ll	-> (parcours_arbre ((ag,x)::(ad,x)::ll) n)
		| _ -> false
	in (parcours_arbre [(ab,0)] (calcul_noir ab 0));;

let ab_bicolore = function ab ->
	(est_abrprof ab) && (racine_noir ab) && (noeud_rouge ab) && (nb_noeud_noir ab);;
	
let ab = Noeud(Noeud(Vide,10,Noir,Vide),15,Noir,Noeud(Vide,17,Rouge,Noeud(Noeud(Vide,22,Rouge,Vide),253,Rouge,Noeud(Vide,254,Rouge,Vide))));;

let rec est_dans = fun ab x ->
	match ab with
	| Vide -> false
	| Noeud(ag,y,_,ad) when y == x -> true
	| Noeud(ag,y,_,ad) when x > y -> (est_dans ad x)
	| Noeud(ag,y,_,ad) -> (est_dans ag x)
	| _ -> failwith "impossible value"

let insert = fun ab x ->
	if (est_dans ab x) then
		ab
	else
		let rec aux = fun abb xx ->
			match abb with
			| Vide -> Noeud(Vide,xx,Rouge,Vide)
			| Noeud(ag,y,_,ad) when x > y -> (aux ad xx)
			| Noeud(ag,y,_,ad) -> (aux ag xx)
			| _ -> failwith "impossible case"

		in aux ab x;;

let color_rouge = function ab ->
	match ab with 
	| Vide -> Vide
	| Noeud(ad,x,Rouge,ag) -> Noeud(ag,x,Rouge,ad)
	| Noeud(ag,x,Noir,ad) -> Noeud(ag,x,Rouge,ad)
	| _ -> failwith "valeur impossible";;

let color_noir = function ab ->
	match ab with 
	| Vide -> Vide
	| Noeud(ad,x,Noir,ag) -> Noeud(ag,x,Noir,ad)
	| Noeud(ag,x,Rouge,ad) -> Noeud(ag,x,Noir,ad)
	| _ -> failwith "valeur impossible";;

let equilibre = function ab ->
	match ab with
	| Vide -> Vide
	| Noeud(Noeud(sabg1,y,Rouge,sabd1),x,Noir,Noeud(sabg2,z,Rouge,sabd2)) -> Noeud( Noeud(sabg1,y,Noir,sabd1),x, Rouge, Noeud(sabg2,z,Noir,sabd2))
	| Noeud(a1,m,Rouge,Noeud(a2,n,Rouge,a3)) -> Noeud(Noeud(a1,m,Rouge,a2),n,Noir,a3)
	| Noeud(Noeud(a1,m,Rouge,a2),n,Rouge,a3) -> Noeud(a1,m,Noir,Noeud(a2,n,Rouge,a3))
	| Noeud(sabg,x,Rouge,sabd) -> Noeud(sabg,x,Noir,sabd)
	| Noeud(a1,m,Noir,Noeud(Noeud(a2,n,Rouge,a3),o,Rouge,a4)) -> Noeud(Noeud(a1,m,Rouge,a2),o,Noir,Noeud(a3,n,Rouge,a4))
	| Noeud(Noeud(a1,n,Rouge,Noeud(a2,o,Rouge,a3)),m,Noir,a4) -> Noeud(Noeud(a1,n,Rouge,a2),o,Noir,Noeud(a3,m,Rouge,a4))
	|_ -> failwith "valeur impossible";;
	
let rec suppr_min = fun ab ->
	match ab with
	| Noeud(Vide,x,_,sad) -> (x, sad)
	| Noeud(sag ,x,_,Vide) -> suppr_min sag
	| Noeud(sag,x, color,sad)  -> let (y, sag2) = (suppr_min sag) in (y, Noeud(sag2,x, color,sad))
		| _ -> failwith "valeur impossible";;

	
let rec suppression = fun ab x ->
	match ab with
	| Vide -> Vide
	| Noeud(Vide,y,_,Vide) when x = y -> Vide
	| Noeud(Vide,y,_,sad) when x = y -> sad
	| Noeud(sag,y,_,Vide)when x = y -> sag
	| Noeud(sag,y,color,sad) when y < x -> Noeud(sag, y, color,(suppression sad x))
	| Noeud(sag,y,color,sad) when y > x-> Noeud((suppression sag x), y, color, sad)
  | Noeud(sag,y,color,sad) -> let (e, sadb) = (suppr_min sad) in Noeud(sag, e, color, sadb)
	|_ -> failwith "impossible case";;

let color_incr = function ab ->
	match ab with
	| Vide -> Vide
	| Noeud(ag,x,Rouge,ad) -> Noeud(ag,x,Noir,ad)
	| Noeud(ag,x,Noir,ad) -> Noeud(ag,x,DoubleNoir,ad)
	| _ -> failwith "impossible case";;

let color_decr = function ab ->
	match ab with
	| Vide -> Vide
	| VideNoir -> Vide
	| Noeud(_,_,Noir,_) -> VideNoir
	| Noeud(ag,x,DoubleNoir,ad) -> Noeud(ag,x,Noir,ad)
	| _ -> failwith "impossible value";;

let rec eq_supp = function ab ->
	match ab with
	| Vide -> Vide
	| Noeud(Noeud(a1,x,DoubleNoir,a2),p,color,Noeud(a3,f,Noir,a4)) -> (color_incr (Noeud((color_decr (Noeud(a1,x,DoubleNoir,a2))),p,color,Noeud(a3,f,Rouge,a4))));;




ab;;
est_abrprof ab;;
racine_noir ab;;
noeud_rouge ab;;	
nb_noeud_noir ab;;
ab_bicolore ab;;
suppression ab 22;;