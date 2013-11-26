(* Tomasz Zakrzewski, WPF *)
(* Praca domowa 2: Tasowanie *)

open List;;
	
let shuffle l1 l2 =
  (* Faktyczna rekurencja; l - pierwsza lista, m - druga lista, res - akumulator wyniku *)
  let rec shuffler l m res =
    if l = [] || m = []
      then rev res
    else
    (
      let lh = hd l and
      lt = tl l and
      mh = hd m and
      mt = tl m 
      in shuffler lt mt (mh::(lh::res))
    )
    in
      (* Procedura zwracająca listę n pierwszych elementów listy. Założenie: n <= dł. listy *) 
      let rec nHead n l res =
	if n = 0
	  then rev res
	else
	  nHead (n-1) (tl l) ((hd l)::res)
      in
	let h = (nHead (min (length l1) (length l2) ) l2 [])
	in shuffler l1 (rev h) [];;
