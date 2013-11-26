(* Arytmetyka - implementacja *)

(* typ *)
type wartosc = Range of (float * float)

(* Procedury pomocnicze *)
(* Procedura sprawdzająca czy przedział jest ciągły *)
let isContinuous x =
	let Range(a, b) = x in
		(a <= b)

(* Procedura sprawdzająca czy przedział "z dziurą" nie stał się [-inf;inf] (czyli ciągły). Zwraca przedział (wejściowy lub [-inf;inf]).
Założenie: x jest przedziałem z dziurą lub wynikiem dodawania takiego przedziału do innego	*)
let normalize x =
	let Range(a, b) = x in
		if a <= b then
			Range(neg_infinity, infinity)
		else
			x

(* Procedura pomocnicza licząca f z pewnych kombinacji (a,b)x(c,d) z użyciem jakiejś funkcji op
Stworzona z myślą o uniknięciu copypasty *)
let combine a b c d op f =
	f (f (op a c) (op a d)) (f (op b c) (op b d))

(* Kontynuacja powyższej procedury - jeszcze większe uogólnienie dla większości przypadków
Warto zauważyć, że procedura zwraca procedurę wymagającą podania op i f*)
let combinator x y =
	let Range(xBegin, xEnd) = x and
	Range(yBegin, yEnd) = y in
		combine xBegin xEnd yBegin yEnd

(* Procedura pomocnicza "naprawiająca" znak 0 w przedziałach (tj. czy przedział "dąży" do 0 od ujemnych czy dodatnich) *)
let fixZeros x =
	match x with
	Range(0., 0.) -> x |
	Range(a, 0.) -> Range(a, (-0.) ) |	(*dla nieciągłych też działa*)
	Range(_, _) -> x	(*wszystko inne, włącznie z [0.;a] bez zmian *)

(* Procedura pomocnicza zwaracająca dla przedziału [-inf;a] lub [a;+inf] koniec przedziału nie będący +-inf.
Założenie: argument jest przedziałem w jednej z 2 wymienionych wyżej postaci *)
let finitePart x =
	let Range(a, b) = x in
		if a = neg_infinity then
			b
		else
			a
(* Konstruktory. Wszystkie zgodne ze specyfikacją, również przyjmują te same założenia *)
let wartosc_dokladnosc x p =
	Range(x -. (x *. p /. 100.), x +. (x *. p /. 100.))
             
let wartosc_od_do x y =
	Range(x, y)

let wartosc_dokladna x =
	Range(x, x)

(* Selektory - analogicznie jak poprzednia sekcja*)
let in_wartosc w x =
	let Range(a, b) = w in
		if isContinuous w then
			a <= x && x <= b
		else
			x <= b || x >= a

let min_wartosc w =
	let Range(a, b) = w in
		if isContinuous w then
			a
		else
			neg_infinity

let max_wartosc w =
	let Range(a, b) = w in
		if isContinuous w then
			b
		else
			infinity
			
let sr_wartosc w =
	if isContinuous w then
		(min_wartosc w +. max_wartosc w) /. 2.
	else
		nan
  
(* Operacje arytmetyczne na niedokładnych wartościach. Znowuż zgodnie ze specyfikacją. *)
let plus x y =
	if not(isContinuous x) && not(isContinuous y) then
		Range(neg_infinity, infinity)
	else
		let Range(xBegin, xEnd) = x and
		Range(yBegin, yEnd) = y in
			let res = Range(xBegin +. yBegin, xEnd +. yEnd) in
				if not(isContinuous x) || not(isContinuous y) then
					normalize res
				else
					res

(* Minus z użyciem plusa *)
let minus x y =
	let Range(yBegin, yEnd) = y in
		plus x (Range(-.yEnd, -.yBegin))
		
let rec razy x y =
	if not(isContinuous x) && not(isContinuous y) then
		Range(neg_infinity, infinity)
	else
		let Range(xBegin, xEnd) = x and
		Range(yBegin, yEnd) = y in
			if not(isContinuous x) || not(isContinuous y) then
			(
				if isContinuous x then
					razy y x	(* Hack, żeby przedział z dziurą był pierwszym argumentem *)
				else
					normalize (Range(combine xEnd xEnd yBegin yEnd ( *. ) (min),	(* Kilka innych hacków, żeby nie klepać wielkiego kodu *)
							combine xBegin xBegin yBegin yEnd ( *. ) (max)))
			)
			else
				Range(combinator x y ( *. ) (min),
					combinator x y ( *. ) (max))	(* "Zwykłe" mnożenie *)
				
	
let podzielic x y =
	if not(isContinuous x) && not(isContinuous y) then	(* Dzielimy nieciągły przez nieciągły *)
		Range(neg_infinity, infinity)
	else	(* Wiemy że może być max 1 nieciągły *)
		let x = fixZeros x and	(* poprawiamy wartości wejściowe (znak zera) *)
		y = fixZeros y in
			let Range(xBegin, xEnd) = x and
			Range(yBegin, yEnd) = y in
				if not(isContinuous x) || not(isContinuous y) then	(* Jeśli któryś jest nieciągły *)
						Range(combinator x y (/.) (min),
							combinator x y (/.) (max))
				else (*już wiemy, że dzielimy ciągły przez ciągły *)
					if (xBegin = neg_infinity || xEnd = infinity) &&
					(yBegin = neg_infinity || yEnd = infinity) then	(*jeśli dzielimy +-inf przez +-inf *)
						let infSgn = (if xBegin = neg_infinity then (-1.) else 1.)
						*. (if finitePart y < 0. then (-1.) else 1.) in	(* Wyliczamy znak nieskończoności - na podstawie znaku niesk. dzielnej oraz znaku krańca "skończonego" dzielnika *)
							if infSgn < 0. then
								Range(neg_infinity, 0.)
							else
								Range(0., infinity)
					else
						if yBegin < 0. && yEnd > 0. then	(* Przedział z zerem, taki z którego powstaną "dziury" *)
							Range(combinator x y (/.) (max),
								combinator x y (/.) (min) )
						else	(* "Zwykłe" dzielenie *)
							Range( combinator x y (/.) (min),
							combinator x y (/.) (max))
;;