(* Tomasz Zakrzewski, numer indeksu: 336079, Projekt: Drzewa Lewicowe *)

(** Typ złączalnej kolejki priorytetowej *)
type 'a queue = Node of ('a queue * 'a * 'a queue * int) | Null;;

(* Pomocnicza funkcja value zwraca dla węzła wartość w nim *)
let value q =
	match q with
	| Null -> raise Empty	(* Prosimy o wartość pustego drzewa *)
	| Node(_, v, _, _) -> v;;

let depth q =
	match q with
	| Null -> -1	(* Sztuczna wartość głębokości Nulla *)
	| Node(_, _, _, d) -> d;;

(** Pusta kolejka priorytetowa *)
let empty = Null;;

(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q p =
	match (q, p) with
	| (Null, Null) -> Null
	| (Null, _) -> p
	| (_, Null) -> q
	| (_, _) ->
		let (Node(l1, v1, r1, r1Depth), q2) =
			(if (value q) < (value p) then (q, p)
			else (p, q) ) in	(* Porządkujemy węzły względem relacji < między wartościami w nich *)
				let partialRes = join r1 q2 in	(* Wynik sklejenia prawego poddrzewa "mniejszego" drzewa z "większym" drzewem *)
					if (depth l1) < (depth partialRes) then	
						Node(partialRes, v1, l1, depth l1 + 1)
					else
						Node(l1, v1, partialRes, depth partialRes + 1);;

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e]
    do kolejki [q] *)
let add a q =
	join (Node(empty, a, empty, 0)) q;;

(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty;;

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min q =
	match q with
	| Null -> raise Empty
	| Node(l, v, r, _) ->
		(v, join l r);;

(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q =
	q = Null;;

(* Testujemy! *)
let sample = empty;;
let sample = add 1 sample;;
let sample = add 2 sample;;
let sample2 = Node(Node(empty, 3, empty, 0), 2, Node(empty, 4, Node(empty, 5, empty, 0), 1), 2);;
let res = join sample sample2;;
let Node(l, _, r, _) = res;;
delete_min res;;
join l r;;


	