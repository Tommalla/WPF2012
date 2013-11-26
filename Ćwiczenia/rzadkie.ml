let isRare n =
	let rec helper x lastBit =
		if x > 0 then
			if (x mod 2) = 1 && lastBit = 1 then
				0
			else
				helper (x/2) (x mod 2)
		else
			1
	in helper n 0;;

let rzadkie n =
	let rec helper x power right lastBit prev older len res =
		if x > 0 then
			let newRes = (if len > 2 then len - 2 else 0) + prev + older
				in
					helper (x / 2) (power*2) (right + (x mod 2) * power) (x mod 2) newRes prev (len + 1) (res +
						(if (x mod 2) = 1 then newRes else 0) +
						(if lastBit != (x mod 2) && lastBit = 1 then (isRare right) else 0) )
		else
			res
	in (helper n 1 0 0 0 0 1 0) + (isRare n);;

rzadkie 42;;
rzadkie 3;;