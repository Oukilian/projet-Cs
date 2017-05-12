type element = int;;
type couleur = Rouge | Noir | Videnoir | DoubleNoir;;
type ab = Vide | Noeud of element * couleur  * ab * ab;;


let rec appartient_a = function x-> function e->
	match x with
	| Noeud(f,_,_,_) when f = e -> true
	| Noeud(f,_,a,b) when e < f -> appartient_a a e
	| Noeud(f,_,a,b) when e > f -> appartient_a b e
	| _ -> false;;

let est_vide = 
	function
	Vide -> false
	|_ -> true;;


let rec est_arb a =
	let rec aux = function
		 [] ->  true
		| (Vide,_,_)::l -> (aux l)
		| (Noeud(e,_,ag,ad),min,max)::l when (e > min) && (e < max) -> (aux((ag,min,e)::((ad,e,max)::l)))
		| _ -> false
		in aux[(a,min_int,max_int)];;

let rac_noi =
	function
	 Vide->true
	|Noeud(f,Noir,_,_) -> true
	| _->false;;



let est_rouge_place a =
	let rec aux l = 
		match l with
		| [] -> true
		| (Vide,_) ::ll -> (aux ll)
		| (Noeud(_,Noir,ag,ad),_)::ll -> (aux((ag,Noir)::((ad,Noir)::ll)))
		| (Noeud(_,Rouge,ag,ad),Noir)::ll -> (aux((ag,Rouge)::((ad,Rouge)::ll)))
		| _->false
		in (aux[(a,Noir)]);;


let nbre_noir a =
	let rec calcul_noir a acc =
		match a with
		| Vide -> acc
		| (Noeud(_,Noir,ag,_)) -> (calcul_noir ag (acc+1))
		| (Noeud(_,Rouge,ag,_)) -> (calcul_noir ag (acc+1))
	and aux l n =
		match l with
	  [] -> true
		| (Vide,x)::ll when (x = n) -> (aux ll n)
		| (Noeud(_,Noir,ag,ad),x)::ll -> (aux ((ag,x+1)::((ad,x+1)::ll))n)
		| (Noeud(_,Rouge,ag,ad),x)::ll -> ((aux((ag,x)::((ad,x)::ll))n))
		| _->false
	in (aux [(a,0)] (calcul_noir a 0));;



let arb = Noeud(15,Rouge,(Noeud(10,Rouge,Vide,Vide)),(Noeud(17,Rouge,Vide,(Noeud(253,Noir,(Noeud(22,Rouge,Vide,Vide)),(Noeud(254,Rouge,Vide,Vide)))))));;

nbre_noir arb;;




(*----TP2----*)


let rec insert a e = 
	match a with
	| Vide -> Noeud(e,Rouge,Vide,Vide)
	| Noeud(v,_,ag,ad) when v > e -> Noeud(v,Rouge,insert ag e,ad)
	| Noeud(v,_,ag,ad) when v < e -> Noeud(v,Rouge,ag,insert ad e)
	| _ -> failwith("Existe déjà");;
(*-let ad = ( equilibre (insert ad e)) in Noeud(v,Rouge,ag,ad))-*)

let rac_bla a =
	function
		| Noeud(a,Rouge,ag,ad)->Noeud(a,Noir,ag,ad);;

let rec equi a =
	match a with
	| Noeud(g,Noir,Noeud(f,Rouge,a1,a2), (Noeud(p,Rouge,a3,a4))) -> (Noeud(g,Rouge,Noeud(f,Noir,a1,a2), (Noeud(p,Noir,a3,a4))))
	| Noeud(g,Noir,Noeud(p,Rouge,Noeud(x,Rouge,a1,a2),a3),Noeud(f,Noir,a4,a5)) -> Noeud(p,Rouge,Noeud(x,Noir,a1,a2),Noeud(g,Noir,a3,Noeud(f,Noir,a4,a5)));;
	(* liste non exhaustive*)

(*--------------TP3-----------*)

(*let rec suppr a ab =
	match a with
	| Vide -> Vide
	| Noeud(g,_,Vide,Vide) when a = g -> Vide
	| Noeud(g,_,ag,ad) when a = g -> Noeud(ad,ag,Vide)
	| Noeud(g,_,Vide,ad) when a = g -> ad
	| Noeud(g,_,ag,Vide) when a = g -> ag;;*)

let rec suppression_min = 
	function
		Noeud(x,c,Vide,ad) -> (x,ad)
	| Noeud(x,c,ag,ad) -> let(y,aa) = (suppression_min ag) in (y,Noeud(x,c,aa,ad))
	| _->failwith "Impossible";;

let rec suppression_abr e a =
	match a with
	| Vide -> Vide
	| Noeud(ee,c,ag,ad) when e < ee -> Noeud(ee,c,(suppression_abr e ag),ad)
	|	Noeud(ee,c,ag,ad) when e > ee -> Noeud(ee,c,ag,(suppression_abr e ad))
	| Noeud(ee,c,ag,ad) -> let (x,aad) = (suppression_min ad) in Noeud(x,c,ag,aad);;

let rec equi_aft_sup a = 
	match a with
	| Noeud(p, Noir, Noeud(x, DoubleNoir,a1,a2), Noeud(f, Noir,Noeud(g, Noir, a3, a4),Noeud(d, Noir, a5, a6))) -> Noeud(p, DoubleNoir, Noeud(x, Noir, a1, a2), Noeud(f, Rouge, Noeud(g, Noir, a3, a4), Noeud(d, Noir, a5, a6)))
	| Noeud(p, Rouge, Noeud(x, DoubleNoir,a1,a2), Noeud(f, Noir,Noeud(g, Noir, a3, a4),Noeud(d, Noir, a5, a6))) -> Noeud(p, DoubleNoir, Noeud(x, Noir, a1, a2), Noeud(f, Rouge, Noeud(g, Noir, a3, a4), Noeud(d, Noir, a5, a6)))
	| Noeud(p, Noir, Noeud(x, DoubleNoir,a1,a2), Noeud(f, Noir,Noeud(g, Noir, a3, a4),Noeud(d, Rouge, a5, a6))) -> Noeud(f, Noir, Noeud(p, Noir, Noeud(x, Noir, a1, a2), Noeud(g,Rouge, a4, a5)), Noeud(d, Noir, a5, a6)) 
	| Noeud(p, Noir, Noeud(x, DoubleNoir,a1,a2), Noeud(f, Noir,Noeud(g, Rouge, a3, a4),Noeud(d, Rouge, a5, a6))) -> Noeud(f, Noir, Noeud(p, Noir, Noeud(x, Noir, a1, a2), Noeud(g,Rouge, a4, a5)), Noeud(d, Noir, a5, a6))
	| Noeud(p, Noir, Noeud(x, DoubleNoir,a1,a2), Noeud(f, Noir,Noeud(g, Noir, a3, a4),Noeud(d, Rouge, a5, a6))) -> Noeud(f, Rouge, Noeud(p, Noir, Noeud(x, Noir, a1, a2), Noeud(g,Noir, a4, a5)), Noeud(d, Noir, a5, a6))
	| Noeud(p, Rouge, Noeud(x, DoubleNoir, a1, a2), Noeud(f, Noir, Noeud(g, Rouge, a3, a4), Noeud(d, Rouge, a5, a6))) -> Noeud(f, Rouge, Noeud(p, Noir, Noeud(x, Noir, a1, a2), Noeud(g, Rouge, a3, a4)), Noeud(d, Noir, a5, a6))
	| Noeud
	| 