(* Tomasz Zakrzewski, WPF *)
(* Homework 2: Shuffling *)
(* The method takes 2 lists and produces a list being a sum of their elements such that for at least one of )
(  the input lists, no two elements from that list are next to each other *)

open List;;

let shuffle l1 l2 =
	(* The actual recursion; l - first list, m - second list, res - the result accumulator *)
	let rec shuffler l m res =
		if l = [] || m = [] then 
			rev res
		else (
			let lh = hd l and
				lt = tl l and
				mh = hd m and
				mt = tl m 
			in shuffler lt mt (mh::(lh::res))
		) 
	in
		(* A function returning n first elements of a list. Assumption: n <= list length *) 
		let rec nHead n l res =
			if n = 0 then 
				rev res
			else
				nHead (n - 1) (tl l) ((hd l)::res)
		in
			let h = (nHead (min (length l1) (length l2) ) l2 [])
			in shuffler l1 (rev h) [];;
