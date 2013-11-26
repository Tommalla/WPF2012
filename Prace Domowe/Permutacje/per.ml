(* Tomasz Zakrzewski, numer indeksu: 336079, zadanie "Permutacje" *)
(* Posiłkowałem się skryptem *)

open List;;

(* Liczymy liczbę potrzebnych zamian podczas wykonywania mergesorta. O( n log n ) *)

(* Pomocnicza funkcja dzieląca listę na 2 (prawie) równe listy zachowując kolejność wejściową *)
let split input =
	let rec helper a id len l =
		if id >= (len / 2) then
			(rev a, l)
		else
			helper ((hd l)::a) (id + 1) len (tl l)
	in helper [] 0 (length input) input;;

(* Procedura pomocnicza łącząca listy (de facto merge z mergesorta) licząca po drodze wynik *)
(* Wiem, powinna być wewnątrz order, aby nie latały "flaki" po kodzie (dobra praktyka programistyczna). *)
(* Chciałem jednak uniknąć strasznego zaśmiecenia wewnątrz order i generalnie zwiększyć czytelność *)
(* Procedura przyjmuje jako argumenty 2 krotki (lista, wynik do tej pory dla niej uzyskany[akumulator]) *)
let merge (l1, res1) (l2, res2) =
	let rec mrg a l1 l2 res len1 len2 =
		match l1 with
		| [] -> ((rev a) @ l2, res)
		| (h1::t1) ->
			match l2 with
			| [] -> ((rev a) @ l1, res)
			| (h2::t2) ->
				if h1 > h2 then
					mrg (h2::a) l1 t2 (res + len1) len1 (len2 - 1)
				else
					mrg (h1::a) t1 l2 res (len1 - 1) len2
	in mrg [] l1 l2 (res1 + res2) (length l1) (length l2);;

(* Właściwa procedura z treści zadania. Wewnątrz po prostu mergesort, zwraca jednak liczbę potrzebnych swapów, *)
(* zamiast posortowanego ciągu *)
let order input =
	let rec sort_cnt (l, res) =
		match l with
		| [] -> ([], 0)
		| [x] -> ([x], 0)
		| _ ->
			let (l1, l2) = split l
				in merge (sort_cnt (l1, 0)) (sort_cnt (l2, 0))
	in
		let (_, result) = sort_cnt (input, 0)
			in result;;

		