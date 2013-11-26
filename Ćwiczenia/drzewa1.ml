(* Tomasz [Tommalla] Zakrzewski 2012 *)
(* Zadanka na drzewa I *)
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;	(*Drzewo binarne*)
type 'a tree = XNode of  ('a tree list * 'a);;	(*Dowolne drzewo*)

let casualTree  = XNode([XNode([XNode([XNode([], 6)], 5); XNode([], 1); XNode([], 8) ], 3); XNode([XNode([], 7)], 4)] , 2);;
let xn = XNode(XNode(XNode([], 6)::XNode([],5)::[],2)::XNode([],3)::XNode([],4)::[], 1);;
let case2 = XNode([XNode([],3);XNode([],4)],2);;
let binaryTree = Node(Node(Node(Node(Leaf, 5, Leaf), 1, Node(Leaf, 8, Leaf)), 3, Node(Leaf, 4, Leaf) ), 2, Node(Node(Leaf, 7, Leaf), 6, Leaf));;
let notLeftist = Node(Node(Leaf, 1, Leaf), 2, Node( Leaf, 3, Node(Leaf, 4, Leaf) ) );;

let rec depth t =
  match t with 
    XNode([], _ ) -> 1 |	(* koniec listy dzieci *)
    XNode(b::e, v) -> max (depth b +1) (if e = [] then 0 else depth (XNode(e, v)) );;
    
depth casualTree;;
depth case2;;

let order node = 
  let rec orderer u res =
    match u with
      XNode([], v) -> v::res |
      XNode(h::t, v) -> orderer (XNode(t, v)) (orderer h res)
  in List.rev(orderer node []);;
  
order casualTree;;

(* Drzewa binarne *)
(* Inorder - lewe, my, prawe *)
let inorder node =
  let rec orderer u res =
    match u with
      Leaf -> res |
      Node( l, v, r ) -> orderer r (v::(orderer l res))
  in List.rev (orderer node []);;
  
inorder binaryTree;;

(* Drzewo ultralewicowe - jeśli kolejne liście mają głębokości nierosnące *)
let ultraleftist node =
  let rec check u depth lastDepth =
    match u with
      Leaf -> 
	if ( lastDepth >= depth || lastDepth = -1 )
	  then (true, depth)
	  else (false, depth) |
      Node(l, v, r) ->
	let (leftRes, leftDepth) = check l (depth+1) lastDepth 
	  in
	    let (rightRes, rightDepth) = check r (depth+1) leftDepth
	      in
		( leftRes && rightRes, (min leftDepth rightDepth) )
   in  
    let (res, garbage) = check node 0 (-1) 
      in res;;
  
ultraleftist binaryTree;;
ultraleftist notLeftist;;
(* seems correct? *)

(* wartościowanie wyrażeń logicznych *)
type expr =
	And of expr * expr |
	Or of expr * expr |
	Not of expr |
	Value of bool;;

let expression = And(Or(Value(true), Value(true)), Not(And(Value(true), Value(true))));;
let simpleExpression = And(Value(true), Value(true));;

let valuate exp =
	let rec valuator e =
		match e with
			And(l, r) ->
				let (leftTrue, leftFalse) = valuator l and
				(rightTrue, rightFalse) = valuator r
					in (leftTrue*rightTrue, leftFalse*(rightFalse+rightTrue) + rightFalse*leftTrue ) |
			Or(l, r) ->
				let (leftTrue, leftFalse) = valuator l and
				(rightTrue, rightFalse) = valuator r
					in ( leftTrue*(rightTrue+rightFalse) + rightTrue*leftFalse, leftFalse*rightFalse ) |
			Not(v) -> 
				let (t, f) = valuator v
					in (f, t) |
			Value(_) -> (1, 1)
	in valuator exp;;
	    
valuate expression;;
valuate simpleExpression;;