(* Tomasz Zakrzewski, nr indeksu: 336079, Zadanie "Tetris" *)

open Array;;

let moduloConst = 1000000007;;

(* Tablica na klocki. Każdy klocek zapisany pole po polu jako pary. Każda para (x,y) oznacza,	*)
(* że dla danego pola (i, j) klocek zajmie pole (i+x, j+y). Tablica zawiera klocki w każdej 	*)
(* możliwej pozycji (obrocie)									*)
let blocks = [|
	(* Klocki typu "I" *)
	[(0, 0); (0, 1); (0, 2); (0, 3)];
	[(0, 0); (1, 0); (2, 0); (3, 0)];
	(* Klocki typu "J" *)
	[(0, 0); (0, 1); (0, 2); (-1, 2)];
	[(0, 0); (0, 1); (1, 1); (2, 1)];
	[(0, 0); (1, 0); (0, 1); (0, 2)];
	[(0, 0); (1, 0); (2, 0); (2, 1)];
	(* Klocki typu "L" *)
	[(0, 0); (0, 1); (0, 2); (1, 2)];
	[(0, 0); (0, 1); (1, 0); (2, 0)];
	[(0, 0); (1, 0); (1, 1); (1, 2)];
	[(0, 0); (0, 1); (-1, 1); (-2, 1)];
	(* Klocki typu "S" *)
	[(0, 0); (0, 1); (1, 1); (1, 2)];
	[(0, 0); (1, 0); (0, 1); (-1, 1)];
	(* Klocki typu "Z" *)
	[(0, 0); (0, 1); (-1, 1); (-1, 2)];
	[(0, 0); (1, 0); (1, 1); (2, 1)];
	(* Klocek typu "O" *)
	[(0, 0); (0, 1); (1, 1); (1, 0)]
|];;

(* Tablica zamieniająca id klocka z obrotami (0-14) na prawdziwe id (0-5) *)
let realId = [| 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 4; 4; 5 |];;

(* Procedura sprawdza, czy dany klocek b da się umieścić w (x,y) *)
let canFit b x y m n matrix =
	List.fold_left (fun a (px, py) ->
		a && (x + px) < m && (y + py) < n &&	(* Czy nie wychodzi "za" planszę *)
		(x + px) >= 0 && (y + py) >= 0 &&	(* Ani "przed" planszę *)
		(not matrix.(x + px).(y + py))) true b;;	(* I wszystkie pola zajmowane przez niego są wolne *)

(* Dodaje lub usuwa klocek, zależnie od wartości f. Założenie: można klocek włożyć *)
let refit b x y matrix f =
	List.iter (fun (px, py) -> matrix.(x + px).(y + py) <- f) b;;

(* Zwraca następne pole (po (x,y)), które jest puste *)
(* Założenie: idziemy wiersz po wierszu od lewego górnego rogu *)
(* Niezmiennik: wszystkie pola w wyższych wierszach lub na lewo *)
(* Od obecnej pozycji są już wypełnione				*)
let nextField x y m n matrix =
	let rec find i j =
		if not matrix.(i).(j) then
			(i, j)
		else
			if i + 1 < m then
				find (i + 1) j
			else
				if j + 1 < n then
					find 0 (j + 1)
				else
					(m, n)
	in find x y;;
	
let tetris m n i j l s z o =
	let res = ref 0	(* referencja na wynik *)
	and matrix = init m (fun _ -> make n false)	(* nasza plansza *)
	and count = [|i; j; l; s; z; o|]	(* Licznik ilości klocków *)
	in
		let increaseRes () =	(* Zwiększanie wyniku *)
			res := (!res + 1) mod moduloConst;
		in
			let rec goTo x y =	(* Faktyczna rekurencja *)
				let (nX, nY) = nextField x y m n matrix
				in
					if (nX, nY) = (m, n) then	(* Jeśli wychodzimy za planszę - znaleźliśmy działający układ *)
						increaseRes ()
					else
						for i = 0 to 14 do	(* Dla każdego klocka, jeśli mamy go > 0 oraz pasuje *)
							if count.(realId.(i)) > 0 && canFit blocks.(i) nX nY m n matrix then begin
								refit blocks.(i) nX nY matrix true;	(* Wstawiamy *)
								count.(realId.(i)) <- count.(realId.(i)) - 1;	(* zmniejszamy jego ilość*)
								goTo nX nY;	(* i schodzimy dalej *)
								refit blocks.(i) nX nY matrix false;	(* A potem cofamy ruch *)
								count.(realId.(i)) <- count.(realId.(i)) + 1;
							end;
						done;()
			in
				if (m * n mod 4 <> 0) || ((i + j + l + s + z + o) * 4 < m *n) then
					0	(* Proste heurystyczne warunki, dla których wynik będzie 0 *)
				else
					begin
						goTo 0 0;
						!res
					end;;