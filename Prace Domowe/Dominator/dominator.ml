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
(* Będziemy trzymali stos trójek (element, wynik dla elementu, id elementu na wejściowej liscie). Niezmiennikiem *)
(* naszego stosu będzie, że elementy od dołu do szczytu są uporządkowane w kolejności nierosnącej. Będziemy szli *)
(* po kolejnych elementach z listy wejściowej i jeśli będą mniejsze od szczytu stosu, będziemy je po prostu dorzucać *)
(* (wynik tymczasowy dla nich to będzie odległość od obecnego szczytu stosu), jeśli będą większe, to dopóki większe, *)
(* będziemy zdejmować elementy ze szczytu stosu i ew. poprawiać wyniki dla nich. Przy zdejmowaniu ze stosu będziemy *)
(* odkładać na listę częściowych wyników pary (indeks, wynik). Na koniec posortujemy tę listę i zmapujemy na listę *)
(* wyników. O( n log n ). *)

(* pomocnicza funkcja rekurencyjnie zdejmująca ze stosu 3-jki aż kolejny element ze szczytu będzie większy (lub pusty stos). *)
(* będziemy jej podawać obecną listę wyników, żeby przy okazji doklejała wyniki *)
let processStack s (elem, id) resList =
	let rec process res =
		match res with
		| ([], l) -> (push (elem, max_int, id) emptyStack, l )
		| (cur, l) ->
			let (topElem, topRes, topId) = top cur in
				if topElem >= elem then
					(push (elem, (if topElem > elem then id-topId else max_int), id) cur, l)
				else
					process (pop cur, (topId, min topRes (id-topId))::l)
	in process (s, resList);;

(* procedura pomocnicza zdejmująca ze szczytu stosu wszystko co na nim zostało i doklejająca do listy wyników *)
let cleanStack s resList =
	let rec cleaner res =
		match res with
		| ([], l) -> l
		| (cur, l) ->
			let (_, _, topId) = top cur in
				cleaner (pop cur, (topId, 0)::l)
	in cleaner (s, resList);;

let dominator input =
	let rec helper l s id res =	(* procedura licząca częściowy wynik - listę par (indeks, wynik) [trzymaną w res], l to obecna lista, s to stos *)
		match l with
		| [] -> (cleanStack s res)
		| (h::t) ->
			let (s, res) = processStack s (h, id) res in
				helper t s (id+1) res
	in
		let partialRes = sort compare (helper input emptyStack 1 []) in
			map (fun (_, r) -> r ) partialRes;;

let test = [4; 3; 1; 4; -1; 2; 1; 5; 7];;
dominator test;;