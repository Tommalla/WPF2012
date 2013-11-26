(* Tomasz Zakrzewski, nr indeksu: 336079, Praca Domowa "Iloczyn" *)
open Array;;

let naprzemienny t =
	let res = (ref 1)
	and cur = (ref 1) in
		for i = 1 to  ((length t) - 1) do
			if t.(i) * t.(i - 1) < 0 then begin
				cur := !cur + 1;
			end else begin
				res := max !cur !res;
				cur := 1;
			end
		done;
	max !cur !res;;
