(* Tomasz Zakrzewski, nr indeksu: 336079, Zadanie "Fastryga" *)

open List;;

type tree =
	Node of int * tree * tree * tree ref|
	Leaf;;

(* compare do sorta *)
let cmp t1 t2 =
	match (!t1, !t2) with
	| (Leaf, Leaf) -> 0
	| (_, Leaf) -> -1
	| (Leaf, _) -> 1
	| (Node(v1, _, _, _), Node(v2, _, _, _)) ->
		if v1 < v2 then
			-1
		else
			if v1 > v2 then
				1
			else
				0;;
	
let fastryguj t =
	let rec createRefList a t = 	(* Procedura pomocnicza tworząca listę referencji do węzłów *)
		match t with
		| Leaf -> a
		| Node(_, Leaf, Leaf, _) -> a
		| Node(_, Leaf, r, _) -> createRefList ((ref r)::a) r
		| Node(_, l, Leaf, _) -> createRefList ((ref l)::a) l
		| Node(_, l, r, _) -> createRefList ((ref l)::(createRefList ((ref r)::a) r)) l
	in
		let rec helper l = (* faktyczna procedura operująca na liście referencji do węzłów posortowanej po wartości w węźle, założenie: lista nie zawiera Leafów *)
			match l with
			| [] -> ()
			| h::[] ->
				let Node(_, _, _, r) = !h (* OCaml niestety nie jest dostatecznie sprytny i nie wie, że h nie może być Leafem... *)
				in
					r := !h	(* kolejny jest ogon, więc wskaźnik sam na siebie *)
			| h::t ->
				let Node(_, _, _, r) = !h	(* j. w. *)
				in
					r := !(hd t);	(* wskaźnik na kolejny "leksykograficznie" wierzchołek *)
					helper t
		in
			helper (sort cmp (createRefList [ref t] t));;	(* wywołujemy helpera na posortowanej liście referencji do wierzchołków *)

