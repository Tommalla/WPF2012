(* Przedkolokwialne klepańsko *)
module type COUNTER = sig
	type counter
	val make : unit -> counter
	val inc : counter -> int
	val reset : unit -> unit
end