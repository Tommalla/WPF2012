(* Tomasz Zakrzewski, numer indeksu 336079 *)
(* Zadanie domowe Dominator *)

open List;;

(* struktura reprezentująca stos *)
type 'a stack = 'a list;;

let emptyStack = [];;

exception Empty;;

(* dodanie na szczyt stosu *)
let push x s =
	x::s;;

(* podejrzenie szczytu stosu *)
let top s =
	match s with
	| [] -> raise Empty
	| (h::t) -> h;;

(* zdjęcie ze szczytu stosu - zwraca stos bez szczytu *)
let pop s =
	match s with
	| [] -> raise Empty
	| (h::t) -> t;;

(* Krótkie objaśnienie algorytmu, żeby poniższy kod był zrozumiały bez zaciemniania komentowaniem każdej linii: *)
(* Będziemy trzymali stos par (element, id elementu na wejściowej liscie). Niezmiennikiem naszego stosu będzie, *)
(* że elementy od dołu do szczytu są uporządkowane w kolejności malejącej. Będziemy szli po kolejnych elementach *)
(* z listy wejściowej i jeśli będą mniejsze od szczytu stosu, będziemy je po prostu dorzucać (wynik dla nich to *)
(* odległość od obecnego szczytu stosu; będziemy wynik dopisywali do listy), jeśli będą większe lub równe, to *)
(* dopóki większe lub równe, będziemy zdejmować elementy ze szczytu stosu. Algorytm powtarzamy dla listy odwróconej.*)
(* Odwracamy jedną z list wynikowych i mapujemy obie listy na minimum z każdej liczby w nich trzymanej oraz dorzucamy *)
(* zera gdzie trzeba. Zł. czasowa: O( n ) *)
	
(* pomocnicza funkcja rekurencyjnie zdejmująca ze stosu pary aż kolejny element ze szczytu będzie większy (lub pusty stos). *)
(* będziemy jej podawać obecną listę wyników, żeby przy okazji doklejała wyniki. Zwraca parę (przerobiony stos, lista wyników) *)
let processStack s (elem, id) resList =
	let rec process res =
		match res with
		| ([], l) -> (push (elem, id) emptyStack, max_int::l )
		| (cur, l) ->
			let (topElem, topId) = top cur in
				if topElem > elem then
					(push (elem, id) cur, (id-topId)::l)
				else
						process (pop cur, l)
	in process (s, resList);;
	

let dominator input =
	let rec helper l s id res =	(* procedura licząca częściowy wynik - "W jedną stronę" *)
		match l with
		| [] -> res
		| h::t ->
			let (s, res) = processStack s (h, id) res in
				helper t s (id+1) res
	in
		let (normal, reversed) = (helper input emptyStack 1 [], helper (rev input) emptyStack 1 []) in
			map (fun x -> (if x = max_int then 0 else x)) (map2 min (rev normal) reversed);;
