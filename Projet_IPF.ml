(* Projet IPF OCaml 2021-2022 --- Teddy ALEXANDRE --- Mai 2022 *)


(* Question 1 *)

(* Création du type string_builder, avec la structure d'arbre binaire *)
type string_builder =
    | Feuille of string * int
    | Noeud of string_builder * string_builder * int;;

(* Création d'un string_builder à partir d'une string sous forme d'une feuille *)
let word s = Feuille(s, String.length s);;

(* Test fonction word *)
let () = assert(word "Bonjour" = Feuille("Bonjour", 7));;

(* Fonction auxiliaire renvoyant la longueur d'une string dans un string_builder *)
let longueur_sb sb = match sb with
    | Feuille(s, n) -> n
    | Noeud(sb1,sb2,n) -> n;;

(* Concaténation de deux string_builders dans un nouveau string_builder *)
let concat sb1 sb2 = Noeud(sb1,sb2,(longueur_sb sb1)+(longueur_sb sb2));;

(* Test fonction concat *)
let () = assert (concat (Noeud(Feuille("Salut", 5), Feuille("Teddy !", 7), 12)) (Noeud(Feuille("Ca", 2), Feuille("Va ?", 4), 6))
 = Noeud(Noeud(Feuille("Salut", 5), Feuille("Teddy !", 7), 12), Noeud(Feuille("Ca", 2), Feuille("Va ?", 4), 6), 18));;


(* Question 2 *)

(* Renvoie le caractère d'indice i dans le string_builder *)
let rec char_at i sb = match sb with
    | Feuille(s,_) -> String.get s i              
    (* Si le string builder est une feuille, on applique directement la fonction get sur la string *)

    | Noeud(sbg,sbd,_) when i >= longueur_sb sbg -> char_at (i-(longueur_sb sbg)) sbd
    (* Si l'indice est plus grand que la taille du string builder de gauche, on cherche dans celui à droite 
       en réindexant correctement *)

    | Noeud(sbg,sbd,_) -> char_at i sbg;;         
    (* Sinon on cherche dans celui à gauche, sans réindexer cette fois *)

(* Test question 2 *)
assert (char_at 4 (Noeud(Feuille("Gat",3), Noeud(Feuille("e",1), Feuille("au",2),3),6)) = 'a');;
assert (char_at 5 (Noeud(Feuille("Gat",3), Noeud(Feuille("e",1), Feuille("au",2),3),6)) = 'u');;


(* Question 3 *)

(* Renvoie le string_builder correspondant à la sous-chaîne voulue *)
let rec sub_string i m sb = match sb with
    | Feuille(s,_) -> Feuille(String.sub s i m, m)
    (* Si le string builder est une feuille, on applique la fonction sub à la chaîne de caractères*)

    | Noeud(sbg, sbd, _) when i+m < longueur_sb sbg -> sub_string i m sbg
    (* Si l'extraction ne dépasse pas la taille du string builder gauche, on cherche à gauche sans réindexer *)

    | Noeud(sbg, sbd, _) when i > longueur_sb sbd -> sub_string i m sbd
    (* Idem si l'extraction se fait à droite en adaptant la condition *)

    | Noeud(sbg, sbd, _) -> let m1 = longueur_sb sbg in
                            Noeud(sub_string i (m1-i) sbg, sub_string 0 (m-(m1-i)) sbd, m);;
    (* Si l'extraction se partage entre les string builders gauche et droite, on prend celle de gauche et on réindice de sorte à obtenir le reste à droite *)

(* Test question 3 *)
assert(sub_string 1 4 (Noeud(Feuille("Gat",3), Noeud(Feuille("e",1), Feuille("au",2),3),6)) 
  = Noeud(Feuille("at", 2), Noeud(Feuille ("e", 1), Feuille("a", 1), 2), 4));;


(* Question 4 *)

(* On écrit une fonction récursive avec deux accumulateurs : le 1er pour le coût
   et le 2e pour la profondeur. On se base ensuite sur la formule fournie dans l'énoncé *)
let cost sb =
    let rec cost_aux acc depth sb = match sb with
        | Feuille(s,n) -> (n*depth)+acc
        (* Si on atteint une feuille, on calcule le coût comme dans l'énoncé *)

        | Noeud(sb1,sb2,n) -> (cost_aux acc (depth+1) sb1)+(cost_aux acc (depth+1) sb2)+acc
        (* Sinon on fait la somme des coûts dans chaque branche, en augmentant la profondeur de 1 *)
    in cost_aux 0 0 sb;;

(* Test question 4 *)
let () = assert(cost (Noeud(Feuille("G", 1), Noeud(Feuille("ATT", 3), Noeud(Feuille("A", 1), Feuille("CA", 2), 3), 5), 6)) = 16);;


(* Question 5 *)

(* Crée une chaîne de caractères aléatoires *)
let rec random_string n = match n with
    | 0 -> ""
    | _ -> let p = Random.int 26 in let c = Char.chr (65 + p) in (Char.escaped c) ^ random_string (n-1);; 

(* Construit un string_builder aléatoirement, vide au départ sauf aux feuilles, jusqu'à atteindre une profondeur i :
   on avance dans l'une où l'autre des branches en fonction des valeurs des booléens.
   Les valeurs -1 sont provisoires, et sont là à titre indicatif, avant le remplissage de l'arbre *)
let rec build_sb_random i bool1 bool2 = match (bool1,bool2,i) with
    | (_, _, i) when i = 0 -> let n = (Random.int 8) + 1 in Feuille(random_string n, n)
    | (false, false, _) -> build_sb_random i (Random.bool ()) (Random.bool ())
    | (false, true, _) -> Noeud(build_sb_random 0 (Random.bool ()) (Random.bool ()), build_sb_random (i-1) (Random.bool ()) (Random.bool ()), -1)
    | (true, false, _) -> Noeud(build_sb_random (i-1) (Random.bool ()) (Random.bool ()), build_sb_random 0 (Random.bool ()) (Random.bool()), -1)
    | (true, true, _) -> Noeud(build_sb_random (i-1) (Random.bool ()) (Random.bool ()), build_sb_random (i-1) (Random.bool ()) (Random.bool ()), -1);;

(* On recalcule les longueurs au sein de l'arbre.
-> J'ai renommé la fonction voulue dans l'énoncé random_string_builder, plus explicite *)
let random_string_builder i = 
    let sb_random = build_sb_random i (Random.bool ()) (Random.bool ()) in 
    let rec recalcule_longueurs sb = match sb with
        | Feuille(s,n) -> Feuille(s,n)
        | Noeud(sb1,sb2,_) -> concat (recalcule_longueurs sb1) (recalcule_longueurs sb2)
    in recalcule_longueurs sb_random;;

(* La fonction renvoie bien un arbre de profondeur donnée *)
random_string_builder 5;;

(* Question 6 *)

(* On écrit une fonction récursive auxiliaire qui balaie le string_builder selon un parcours infixe :
   string_builder gauche -> racine -> string_builder droite.
   L'accumulateur correspond à la liste des strings dans les feuilles du string_builder *)
let list_of_string sb =
    let rec list_of_string_aux liste_acc sb = match sb with
        | Feuille(s,n) -> s::liste_acc
        | Noeud(sbg,sbd,n) -> (list_of_string_aux liste_acc sbg)@liste_acc@(list_of_string_aux liste_acc sbd)
    in list_of_string_aux [] sb;;

(* Test question 6 *)
let () = assert(list_of_string (Noeud(Feuille("G", 1), Noeud(Feuille("ATT", 3), Noeud(Feuille("A", 1), Feuille("CA", 2), 3), 5), 6)) = ["G"; "ATT"; "A"; "CA"]);;


(* Question 7 *)

(* Liste infixe des feuilles de l'arbre avec map *)
let list_of_leaves sb = List.map (fun x -> word x) (list_of_string sb);;

(* Calcule récursivement l'indice indiquant où la suppression de a et b s'est effectuée 
   et où l'insertion de la concaténation de a et b doit se faire dans la liste *)
let rec find_pos a b l = match l with
    | [] -> -1
    | [x] -> -1
    (* Si la liste est de longueur < 2, on renvoie une valeur arbitraire *)
    | t1::t2::q when t1=a && t2=b -> 0
    | t1::t2::q -> 1 + (find_pos a b (t2::q));;

(* Supprime récursivement a et b de la liste (on convient que a et b sont consécutifs) *)
let rec supprimer a b l = match l with
    | [] -> []
    | [x] -> []
    | t1::t2::q when t1=a && t2=b -> q
    | t1::t2::q -> t1::(supprimer a b (t2::q));;

(* Insère récursivement un élément x en k-ème position dans une liste l *)
let rec inserer k x l = match l with
    | [] -> [x]
    | t::q when k = 0 -> x::l
    | t::q -> t::(inserer (k-1) x q);;

(* Tests partiels question 7 *)
let () = assert(find_pos 3 4 [1;2;3;4;5] = 2);;
let () = assert(supprimer 4 5 [1;2;3;4;5] = [1;2;3]);;
let () = assert(inserer 5 6 [1;2;3;4;5] = [1;2;3;4;5;6]);;


(* Trouve les 2 éléments qui réalisent le coût minimum de concaténation dans le string_builder *)
let rec find_min_cost sb l elt1 elt2 cost_min =  match l with
    | [] -> (elt1,elt2)
    | [x] -> (elt1,elt2)
    | sb1::sb2::q -> let poids = cost (concat sb1 sb2) in 
                     if poids < cost_min then find_min_cost sb (sb2::q) sb1 sb2 poids
                     else find_min_cost sb (sb2::q) elt1 elt2 cost_min;;

(* Implémentation de l'algorithme suggéré dans l'énoncé *)
let balance sb =
    let rec balance_aux sb liste_sb = match liste_sb with
        | [] -> failwith "Taille de la liste nulle / Arbre vide"
        | [x] -> x
        | [sb1; sb2] -> concat sb1 sb2
        | t1::t2::q -> let (a,b) = find_min_cost sb liste_sb (word "") (word "") max_int in
                       let new_liste_sb = supprimer a b liste_sb and pos_insert = find_pos a b liste_sb in
                       balance_aux sb (inserer pos_insert (concat a b) new_liste_sb)
    in balance_aux sb (list_of_leaves sb);;

let () = assert(balance (Noeud(Feuille("G", 1), Noeud(Feuille("ATT", 3), Noeud(Feuille("A", 1), Feuille("CA", 2), 3), 6), 7)) 
= Noeud (Noeud (Feuille ("G", 1), Feuille ("ATT", 3), 4), Noeud (Feuille ("A", 1), Feuille ("CA", 2), 3), 7));;

(* Question 8 *)

(* Génère un grand nombre d'arbres et calcule les gains avec l'algorithme d'équilibrage *)
let liste_gains n = 
    let rec liste_gains_aux n accu = match n with
    | 0 -> accu
    | _ -> 
        let tree = random_string_builder (Random.int 16) in
        let balanced_tree = balance tree in
        let cost_init = cost tree and cost_balanced = cost balanced_tree in
        liste_gains_aux (n-1) ((cost_init - cost_balanced)::accu)
    in liste_gains_aux n [];;

(* Moyenne d'une liste d'entiers avec fold_left *)
let average l =
    let (sum, n) = List.fold_left (fun (sum, n) x -> (sum + x, n + 1)) (0, 0) l
    in float_of_int sum /. (float_of_int n);;

(* Pour calculer la médiane, il faut disposer d'une liste triée :
   on applique le tri fusion, efficace en O(n*log(n)) *)
let rec divise l = match l with
    | [] -> [],[]
    | [x] -> [x],[]
    | t1::t2::q -> let l1,l2 = divise q in (t1::l1),(t2::l2);;

let rec fusion l1 l2 = match (l1,l2) with
    | l1,[] -> l1
    | [],l2 -> l2
    | t1::q1,t2::q2 when t1 < t2 -> t1::(fusion q1 l2)
    | t1::q1,t2::q2 -> t2::(fusion l1 q2);;

let rec tri_fusion l = match l with
    | [] -> []
    | [x] -> [x]
    | t::q -> let l1,l2 = divise l in fusion (tri_fusion l1) (tri_fusion l2);;

(* Renvoie l'élément d'indice n dans une liste *)
let rec find_nth l n = match l with
    | [] -> failwith "pas d'élément en n-eme position / indice invalide"
    | t::q when n = 0 -> t
    | t::q -> find_nth q (n-1);;

(* Calcule la médiane d'une liste triée d'entiers *)
let median l = let l_sorted = tri_fusion l and n = List.length l in match n with
    | n when n mod 2 = 1 -> float_of_int (find_nth l_sorted (n/2)) 
    | n -> ((float_of_int (find_nth l_sorted (n/2-1))) +. (float_of_int(find_nth l_sorted (n/2)))) /. 2.;;

(* Minimum d'une liste *)
let rec min_liste l = List.fold_left (fun x acc -> min x acc) max_int l;;

(* Maximum d'une liste *)
let rec max_liste l = List.fold_left (fun x acc -> max x acc) min_int l;;

(* Renvoie l'ensemble des données liées à l'algorithme d'équilibrage *)
let calcule_gains list_gains = (min_liste list_gains, max_liste list_gains, average list_gains, median list_gains);;

(* On fournit une liste en paramètre *)
calcule_gains (liste_gains 1000);;