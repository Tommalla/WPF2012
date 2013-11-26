(* Tomasz Zakrzewski, indeks: 336079, WPF, Praca Domowa 5: Najczęstszy *)

open List;;

let najczestszy l =
	let l = sort compare l in
		let (best, bestCnt, last, cnt) = (
			fold_left (fun (best, bestCnt, last, cnt) h -> (
				if h = last then		(* jeśli kolejny element jest taki sam jak poprzednio analizowany *)
					(best, bestCnt, last, cnt+1)	(* zwiększamy jego licznik *)
				else
					if bestCnt < cnt then	(* nowy element - trzeba rozstrzygnąć czy poprzedni był lepszy niż poprzedni wynik i odpowiednio przekazać krotkę*)
						(last, cnt, h, 0)
					else
						(best, bestCnt, h, 0)
				)) (0., min_int, hd l, 0) (tl l) )	(* zaczynamy od jakiegoś śmiecia, abstrakcyjnej ilości wystąpień -inf, głowy listy z ilością wystąpień 1, lecimy po ogonie *)
		in
			if bestCnt < cnt then	(* ostatnio rozważany elem. może być najczęstszym i musimy to sprawdzić na końcu *)
				last
			else
				best;;

		