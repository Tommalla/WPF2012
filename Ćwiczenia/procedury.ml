(* 31 X 2012 *)
open List;;
(* 1. Parzyste elementy listy *)
let even l =
	 rev (fst (fold_left (fun (a, c) h -> if (c mod 2) = 1 then (h::a, (c+1)) else (a, (c+1)) ) ([], 0) l ));;

let sample = [1;2;3;4;5;6];;
even sample

(* QuickSort *)
let rec quicksort l =
	match l with
	[] -> [] |
	h::t -> (quicksort(filter (fun x -> x < h) t)) @ [h] @ (quicksort(filter (fun x -> x >= h) t));;

let unordered = [5;6;2;0;1;3;8;10;12;11;3;1];;
quicksort unordered;;

(*3. Wzrost - największą rosnącą podlistę *)
(* Największy rosnący podciąg *)

(* 4. "Dopóĸi - przy tablicy *)

(* 5. Nawiasowanie *)
(* let brace l =
	if (length l) mod 2 = 1 then *)

(* 6. roznice *)
(* 7. Buduj permutację - głupio ;D *)
(*let build_permutation l =
	fold_left (fold_left)*)
;;