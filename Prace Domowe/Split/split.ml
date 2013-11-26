(* Tomasz Zakrzewski, nr indeksu: 336079 	*)
(* Praca domowa numer 3: Split			*)
open List;;

(* Procedura z treści zadania *)
let split l =
	(* Procedura splitModulo dla listy x wylicza listę elementów o pozycjach, z których numerów resza z dzielenia przez m jest równa r *)
	let splitModulo x m r =
		rev (fst (fold_left (fun (a, c) h -> if (c mod m) = r then (h::a, c+1) else (a, c+1)) ([], 0) x))
	in
		(splitModulo l 2 0, splitModulo l 2 1);;

let sample = [1; 2; 3; 4; 5];;
split sample;;
	