(* Tomasz Zakrzewski, nr indeksu: 336079 Zadanie "Origami" *)

open List;;

(* Kwadrat *)
let square a =
	a *. a;;

(* Odległość w metryce Euklidesowej *)
let distance (x1, y1) (x2, y2) =
	sqrt ( (square (x1 -. x2)) +. (square (y1 -. y2)) );;

let det (x1, y1) (x2, y2) (x3, y3) =
	x1 *. y2 +. y1 *. x3 +. x2 *. y3 -.
	(x3 *. y2 +. y3 *. x1 +. x2 *. y1);;
	
(* Sprawdza czy punkt jest po prawej stronie zgięcia *)
let rightTo (x, y) (x1, y1) (x2, y2) =
	det (x1, y1) (x2, y2) (x, y) < 0.;;
	
(* Zwraca punkt symetryczny do (x,y) wzg. prostej ustalonej przez (x1, y1) (x2, y2) *)
let symmetricPoint (x1, y1) (x2, y2) (x, y) =
	if y1 = y2 then
		(x, 2. *. y1 -. y)
	else
		if x1 = x2 then
			(2. *. x1 -. x, y)
		else
			let a1 = (y1 -. y2) /. (x1 -. x2)
			and a2 = (x2 -. x1) /. (y1 -. y2) in
				let b1 = y2 -. a1 *. x2
				and b2 = y -. a2 *. x in	(* Proste prostopadłe: [p1; p2] i prostopadła prosta przech. przez p *)
					let xs = (b2 -. b1) /. (a1 -. a2) in
						let ys = a2 *. xs +. b2 in	(* (xs, ys) - punkt przecięcia prostej prostopadłej z [p1; p2] *)
							(2. *. xs -. x, 2. *. ys -. y);;

type point = float * float;;

type kartka = point -> int;;

let prostokat (x1, y1) (x2, y2) =
	fun (x, y) ->
		if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1
		else 0;;

let kolko (xs, ys) r =
	fun (x, y) ->
		if (distance (x, y) (xs, ys)) <= r then 1
		else 0;;
		
let zloz (x1, y1) (x2, y2) paper =
	fun (x, y) ->
		let (xs, ys) = symmetricPoint (x1, y1) (x2, y2) (x, y) in
			if (rightTo (x, y) (x1, y1) (x2, y2)) then
				0
			else
				if (xs = x && ys = y) then
					paper (x, y)
				else
					(paper (x, y)) + (paper (xs, ys));;

let skladaj input paper =
	let rec helper l res=
		match l with
		| [] -> res
		| (p1, p2)::t ->
			helper t (zloz p1 p2 res)
	in helper input paper;;
	
(* Testowanie *)
let pr = prostokat (1., 0.) (6., 4.);;
pr (3., 4.);;
pr (0., 0.);;
let p2 = (2., 4.);;
let p1 = (5., -2.);;
let p3 = (3., 4.);;
let p4 = (3., 0.);;
let p = (3.1, 0.5);;

let pr2 = prostokat (-3., -3.) (3., 3.);;
let zlozenia2 = [((0., -4.), (0., 4.)); ((4., 0.), (-4., 0.))];;
let pSym = symmetricPoint p1 p2 p;;
symmetricPoint p1 p2 pSym;;
symmetricPoint p2 p1 pSym;;

let zl = zloz p1 p2 pr;;
zl p;;

let zlozenia = [(p1, p2); (p3, p4)];;
let zl = skladaj zlozenia pr;;
zl p;;
let zl = skladaj zlozenia2 pr2;;

zl (-2., -2.);;
hd zlozenia2;;
symmetricPoint (0., 4.) (0., -4.) (-2., -2.);;
zloz (fst (hd zlozenia2)) (snd (hd zlozenia2)) pr2 (-2., -2.);;
rightTo (-2., -2.) (0., 4.) (0., -4.);;
rightTo (-2., -2.) (-4., 0.) (4., 0.);;

let k = kolko (3., 2.) 2.;;
k (2., 2.);;
zloz (4., 0.) (2., 4.) k (2., 2.);;