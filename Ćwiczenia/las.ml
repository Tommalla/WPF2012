(* Tomasz [Tommalla] Zakrzewski, 2012 *)
(* Najdłuższy podciąg rosnący *)

module type OrderedList =
	sig
		type t
		val compare : t -> t -> int
	end;;

module OrderedList =
	struct
		type t = int
		let compare = compare(*a b = compare (List.hd a) (List.hd b)*)
	end;;
	
module ListMap = Map.Make (OrderedList);;

let lower_bound x map =
	try
		ListMap.find x map
	with Not_found ->
			let (_, _, tmp) = ListMap.split x map in
				try
					let (_, res) = ListMap.min_binding tmp in
						res
				with Not_found -> [];;
					
let las input =
	let map = ref ListMap.empty in
		let addValue (x, id) =
			let l = lower_bound x !map in
				print_string "lower_bound dla ";
				print_int x;
				print_string " to ";
				if l <> [] then begin
					print_int (fst (List.hd l));
					print_char '\n';
					map := ListMap.remove (fst (List.hd l)) !map;
					map := ListMap.add x ((x, id)::l) !map;
				end
				else begin
					map := ListMap.add x [(x, id)] !map;
					print_string "lista_pusta\n";
				end
				
		in
			List.iteri (fun id x -> addValue (x, id) ) input;
			snd (List.fold_left (fun (lastId, a) l ->
				let (x, id) = List.hd (List.filter (fun (x, id) -> id < lastId) l) in
					(id, x::a) ) (max_int, []) (ListMap.fold (fun _ h a -> h::a) !map []));;

let sample = [1; 3; (-1); 2; 0; 1; 5; 7; 8; 9; 2; 3; 1];;
las sample;;