type element = int
type couleur = Rouge | Noir | DoubleNoir
type ab = Vide | VideNoir | Noeud of ab * element * couleur * ab
type element_augmente = Min | Element of element | Max
val compar : element_augmente -> element_augmente -> bool
val appartient_a : element -> ab -> bool
val est_vide : ab -> bool
val est_abr : ab -> bool
val est_abrprof : ab -> bool
val racine_noir : ab -> bool
val noeud_rouge : ab -> bool
val nb_noeud_noir : ab -> bool
val ab_bicolore : ab -> bool
val ab : ab
val est_dans : ab -> element -> bool
val insert : ab -> element -> ab
val color_rouge : ab -> ab
val color_noir : ab -> ab
val equilibre : ab -> ab
val suppr_min : ab -> element * ab
val suppression : ab -> element -> ab
val color_incr : ab -> ab
val color_decr : ab -> ab
val eq_supp : ab -> ab
