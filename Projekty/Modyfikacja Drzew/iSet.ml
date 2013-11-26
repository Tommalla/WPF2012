(* Tomasz Zakrzewski, nr indeksu: 336079, Zadanie Modyfikacja Drzew *)

(* Rzeczy związane z przedziałami *)
(* cmp porównujący 2 przedziały *)
let cmp (xb, xe) (yb, ye) =
	if xe < yb then
		-1
	else if ye < xb then
		1
	else
		0	(* niepusto się przecinają *)

(* Funkcja sumująca 4 liczby. Wynikiem jest max_int, jeśli suma przekracza zakres lub suma *)
let sum a b c d =
	if max (max a b) (max c d) = max_int ||
		a >= max_int - b ||
		c >= max_int - d ||
		a + b >= max_int - c - d then
			max_int
		else
			a + b + c + d

(* Drugi compare potrzebny do porówywania przedziałów z uwzględnieniem zazębień.
Zrobiłbym z tego jedną funkcję, ale pierwotnie zapomniałem o zazębieniach i
teraz byłoby pełno roboty ze  zmienianiem. Poza tym kod by się pokomplikował *)
let cmp2 (xb, xe) (yb, ye) =
	if sum xe 1 0 0 < yb then
		-1
	else if sum ye 1 0 0 < xb then
		1
	else
		0

(* Rzeczy zwiążane z samym drzewem *)
type t =
	| Node of t * (int * int) * t * int * int (* (l, (begin, end), r, h, bel) - bel - wartość do liczenia below - ile po lewo w dół *)
	| Empty

let empty =
	Empty
	
let is_empty s =
	s = Empty

let elements s =
	let rec loop s res =
		match s with
		| Node (l, v, r, _, _) ->
			loop l (v::(loop r res))
		| Empty -> res
	in loop s []

let mem x s =
	let rec loop s =
		match s with
		| Node (l, v, r, _, _) ->
			let c = cmp (x, x) v in
				c = 0 || loop (if c < 0 then l else r)
		| Empty -> false
	in loop s

let height = function
	| Node (_, _, _, h, _) -> h
	| Empty -> 0

let belV = function
	| Node (_, _, _, _, bel) -> bel
	| Empty -> 0

(* Rozmiar przedziału w wierzchołku *)
let sizeV = function
	| Node (_, (vb, ve), _, _, _) -> abs (ve - vb) + 1
	| Empty -> 0

(* Zwraca wartość wierzchołka. Zał - niepusty! *)
let value = function
	| Node (_, v, _, _, _) -> v
	| Empty -> assert false

	
let make l v r =
	Node (l, v, r, max (height l) (height r) + 1, sum  (belV l) (sizeV l) (belV r) (sizeV r) )
	
(* Balance lekko zmieniony *)
let bal l v r =
	let hl = height l
	and hr = height r in
		if hl > hr + 2 then
			match l with
			| Node (ll, lv, lr, _, _) ->
				if height ll >= height lr then make ll lv (make lr v r)
				else
					(match lr with
					| Node (lrl, lrv, lrr, _, _) ->
						make (make ll lv lrl) lrv (make lrr v r)
					| Empty -> assert false)
			| Empty -> assert false
		else if hr > hl + 2 then
			match r with
			| Node (rl, rv, rr, _, _) ->
				if height rr >= height rl then make (make l v rl) rv rr
				else
					(match rl with
					| Node (rll, rlv, rlr, _, _) ->
						make (make l v rll) rlv (make rlr rv rr)
					| Empty -> assert false)
			| Empty -> assert false
		else make l v r

(* Funkcja dodaje do drzewa przedział z założeniem, że jest rozłączny ze wszystkim w drzewie *)
let rec add_separate x s =
	match s with
	| Node(l, v, r, _, _) ->
		let c = cmp x v in
			if c <= 0 then
				let nl = add_separate x l in
					bal nl v r
			else
				let nr = add_separate x r in
					bal l v nr
	| Empty -> Node(Empty, x, Empty, 1, 0)
		
(* Join. Założenie - wszystkie przedziały z obu drzew parami rozłączne *)
let rec join l v r =
	match (l, r) with
	| (Empty, _) -> add_separate v r
	| (_, Empty) -> add_separate v l
	| (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
		if lh > rh + 2 then
			bal ll lv (join lr v r)
		else if rh > lh + 2 then
			bal (join l v rl) rv rr
		else
			make l v r


let rec joinTwo s a =
	match (s, a) with
	| (Node(sl, sv, sr, sh, _), Node(al, av, ar, ah, _)) ->
		if sh > ah + 2 then
			bal sl sv (joinTwo sr a)
		else
			bal (joinTwo s al) av ar
	| (Empty, _) -> a
	| (_, Empty) -> s

	
let split x s =
	let rec loop s =
		match s with
		| Node (l, v, r, _, _) ->
			let c = cmp (x, x) v in
				if c = 0 then
					let (vb, ve) = v in
						((if vb = x then l else add_separate (vb, x - 1) l), true, (if ve = x then r else add_separate (x + 1, ve) r))
				else if c < 0 then
					let (resl, found, nl) = loop l in
						(resl, found, join nl v r)
				else
					let (nr, found, resr) = loop r in
						(join l v nr, found, resr)
		| Empty -> (Empty, false, Empty)
	in loop s

let remove (xb, xe) s =
	let (nl, _, _) = split xb s
	and (_, _, nr) = split xe s in
		joinTwo nl nr

let rec findIntersection x s =
	match s with
	| Node(l, v, r, _, _) ->
		let c = cmp2 x v in
			if c = 0 then
				let (resb, _) = findIntersection x l
				and (_, rese) = findIntersection x r
				and (xb, xe) = x
				and (vb, ve) = v in
					(min resb (min xb vb), max rese (max xe ve))
			else if c < 0 then
				findIntersection x l
			else
				findIntersection x r
	| Empty -> x
		
let add x s =
	let x = findIntersection x s in
		let s = remove x s in
			add_separate x s

let below x s =
	let rec loop s =
		match s with
		| Node(l, v, r, _, _) ->
			let c = cmp (x, x) v in
				if c = 0 then
					let (vb, _) = v in
						sum (belV l) (sizeV l) (x - vb + 1) 0 (* Bingo! *)
				else if c < 0 then
					loop l
				else
					sum (belV l) (sizeV l) (sizeV s) (loop r)
		| Empty -> 0
	in loop s

let iter f =
	let rec loop = function
		| Empty -> ()
		| Node (l, v, r, _, _) -> loop l; f v; loop r
	in loop

let fold f s res =
	let rec loop res = function
		| Empty -> res
		| Node (l, v, r, _, _) ->
          loop (f v (loop res l)) r in
  loop res s
;;