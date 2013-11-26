(* Testowanie *)
 let rec show a t =
 match t with
 | Leaf -> a
 | Node(v, l, r, s) ->
 let Node(u, _, _, _) = !s
 in
 show (show ((v, u)::a) l) r;;

let sample = Node(1, Node(3, Node(2, Leaf, Leaf, ref Leaf), Node(6, Node(0, Leaf, Leaf, ref Leaf), Leaf, ref Leaf), ref Leaf),
 Node(10, Node(7, Leaf, Leaf, ref Leaf), Node(8, Node(1, Leaf, Leaf, ref Leaf), Node(4, Leaf, Leaf, ref Leaf), ref Leaf), ref Leaf ), ref Leaf);;
fastryguj sample;;
show [] sample;;