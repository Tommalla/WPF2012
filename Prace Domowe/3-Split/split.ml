(* Tomasz Zakrzewski, WPF *)
(* Homework 3: Split *)
(* The method takes a list and splits it into two - one with elements of even indexes and the second with the rest (odd) *)
open List;;

let split l =
	(* splitModulo for a list x computes the list of elements with indexes idx such that idx mod m = r *)
	let splitModulo x m r =
		rev (fst (fold_left (fun (a, c) h -> if (c mod m) = r then (h::a, c+1) else (a, c+1)) ([], 0) x))
	in
		(splitModulo l 2 0, splitModulo l 2 1);;

let sample = [1; 2; 3; 4; 5];;
split sample;;
	