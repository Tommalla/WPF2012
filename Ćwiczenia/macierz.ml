(* Tomasz [Tommalla] Zakrzewski, 2013 *)
(* Dynamiczna suma na prostokącie *)

let calculateMatrix matrix =
	let m = Array.length matrix
	and n = Array.length matrix.(0) in begin
		for i = 0 to (m - 1) do
			for j = 0 to (n - 1) do
				matrix.(i).(j) <- matrix.(i).(j) + (if i > 0 then matrix.(i - 1).(j) else 0)
				+ (if j > 0 then matrix.(i).(j - 1) else 0) - (if i > 0 && j > 0 then matrix.(i - 1).(j - 1) else 0);
			done;
		done;
	end;();;

let sum a b c d matrix =
	matrix.(d).(c) - (if b > 0 then matrix.(b - 1).(d) else 0) - (if a > 0 then matrix.(c).(a - 1) else 0)
	+ (if a > 0 && b > 0 then matrix.(b - 1).(a - 1) else 0);;

