(* Tetris - testy *)
tetris 4 4 4 0 0 0 0 0;;
tetris 7 7 0 0 0 0 0 0;;
tetris 4 4 4 0 0 0 0 2;;
tetris 4 7 3 2 0 0 1 2;;
tetris 3 4 0 2 0 0 1 0;;
tetris 4 4 1 2 0 0 1 0;;

(*
let m = init 6 (fun _ -> make 6 false);;

for i = 0 to 14 do
	refit blocks.(i) 2 2 m true;
	printMatrix 6 6 m;
	refit blocks.(i) 2 2 m false;
done*)