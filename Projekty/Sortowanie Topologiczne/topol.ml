(* Tomasz Zakrzewski, nr indeksu 336079, Zadanie "Sortowanie Topologiczne" *)

open PMap;;

exception Cykliczne

(* Pomocnicza procedura łącząca 2 posortowane listy zachowując posortowanie	*)
(*				i usuwając powtórzenia; używa std komparatora	*)
let merge a b =
	let rec loop a b res =
		match (a, b) with
		| ([], _) -> (List.rev res) @ b
		| (_, []) -> (List.rev res) @ a
		| (ah::at, bh::bt) ->
			let c = compare ah bh in
				if c = 0 then
					loop a bt res
				else
					if c < 0 then
						loop at b (ah::res)
					else
						loop a bt (bh::res)
	in loop a b []

(* Pomocnicza procedura licząca ilość wierzchołków w grafie zgodnym z opisem z zadania *)
let getNodesQty l =
	List.length (
		List.fold_left (fun a hl -> merge a hl) [] (
			List.map (fun (hv, hl) -> List.sort compare (hv::hl)) l))

let topol l =
	let s = ref (List.fold_left (fun a (hv, hl) -> add hv hl a) empty l)	(* Set do wyłuskiwania listy sąsiadów danego wierzchołka *)
	and qs = ref (List.fold_left (fun a (hv, _) -> add hv 0 a) empty l)	(* Set pamiętający dla każdego wierzchołka jego stopień wejściowy (pierwotnie 0) *)
	and res = ref []	(* Miejsce na wynikową listę *)
	and q = ref [] in	(* "kolejka" de facto będąca stosem (kolejność nie ma znaczenia, ważne żeby wrzucać wierzchołki na q jeśli mają inDeg = 0) *)
		let neighbours v =
			try
				find v (!s)	(* Funkcja wyłuskująca sąsiadów wierzchołka *)
			with Not_found -> []
		and qty v =
			try
				find v (!qs)	(* Analogicznie wyłuskująca stopień wejściowy *)
			with Not_found -> 0
		in
			let modifyQty v x = qs := add v ((qty v) + x) (remove v (!qs)) in begin	(* Funkcja zmieniająca inDeg wierzchołka v *)
				List.iter (fun (hv, hl) ->
					(List.iter (fun v -> modifyQty v 1) hl)) l;	(* Inicjalizujemy stopnie wejściowe *)

				iter (fun v qt -> begin
					if qt = 0 then
						q := v::(!q);
					end;()
				) (!qs);	(* Wrzucamy na "kolejkę" wszystkie wierzchołki ze stopniem wejściowym 0 *)
					
				while not ((!q) = []) do	(* dopóki kolejka niepusta *)
					let v = List.hd (!q)	(* Front *)
					and nl = neighbours (List.hd (!q)) in begin	(* Lista sąsiadów *)
						q := List.tl (!q);	(* pop *)
						res := v::(!res);	(* Dodajemy wierzchołek do wyniku *)
						List.iter (fun u -> modifyQty u (-1)) nl; (* zmniejszamy inDegi sąsiadów o 1 *)
						List.iter (fun u -> begin
							if qty u = 0 then
								q := u::(!q);
						end; () ) nl;	(* dodajemy do kolejki wszystkich sąsiadów z inDegiem 0. Nie dodamy żadnego 2 razy,
									bo jeśli znów go napotkamy, to ustawimy mu inDeg na -1, potem -2 itd. *)
					end;
				done
			end;
			if ((List.length (!res)) <> (getNodesQty l)) then	(* Jeśli jakiś wierzchołek nie został przerobiony - nie ma go w wyniku *)
				raise Cykliczne
			else
				List.rev (!res);;

