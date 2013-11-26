(* Tomasz [Tommalla] Zakrzewski, 2013 *)
(* Wydawanie reszty najmniejszą możliwą ilością monet *)

let giveChange input sum =
	let tab = Array.make (sum + 1) 0 in
		let addNominal x =
			for i = sum downto 0 do
				if (tab.(i) > 0 && i + x <= sum) || i = 0 then begin
					if tab.(i + x) = 0 then
						tab.(i + x) <- tab.(i) + 1
					else
						tab.(i + x) <- min (tab.(i) + 1) tab.(i + x)
				end;
			done;()
		in
			List.iter (fun x -> addNominal x) input;
			tab;;

let sample = [2; 2; 2; 3; 5; 3; 2; 1];;
giveChange sample 15;;