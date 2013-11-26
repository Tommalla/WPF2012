open Printf;;
open Scanf;;
open ISet;;

let rec print_list = function
  | [] -> printf "\n";
  | h::t -> printf "(%d,%d) " (fst h) (snd h); print_list t;;

let tree = (ref empty);;

let n = (scanf "%d\n" (fun a -> a));;

for i=0 to n-1 do
  tree := add (scanf "%d %d\n" (fun a b -> (a,b))) !tree
done;

for i=0 to n-1 do
  tree := remove (scanf "%d %d\n" (fun a b -> (a,b))) !tree
done;

print_list (elements !tree);;
