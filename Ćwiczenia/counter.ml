module Counter : COUNTER = struct
	type counter = int ref
	let l = ref []
	let make () =
		let p = ref 0 in
			l := p::(!l);
			p
	let inc c =
		c := !c + 1;
		!c
	let reset () =
		List.fold_left (fun _ p -> p := 0) () (!l)
end;;