(* Testy *)

let sample = [|
	[| 1; 2; 4 |];
	[| 1; 3; 1 |];
	[| 6; 2; 3 |];
	[| 8; 2; 1 |]
	|];;

calculateMatrix sample;;

sample;;

sum 0 0 2 3 sample;;
sum 1 1 1 1 sample;;