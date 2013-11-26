(* Tomasz Zakrzewski, Homework I *)
(* This method counts the number of zeroes in decimal representation of all numbers in [a;b] *)
let zeros a b =
	(* Helper method to count the number of zeros in x *)
	let rec countZeros x acc =
		if x <= 0 then
			acc
		else
			countZeros (x / 10) (acc + (if (x mod 10) = 0 then 1 else 0))
	in
	(* The actual recursion - n is the argument *)
		let rec helper n res =
			if n > b then
				res
			else
				helper (n + 1) (res + (countZeros n 0))
		in helper a 0;;