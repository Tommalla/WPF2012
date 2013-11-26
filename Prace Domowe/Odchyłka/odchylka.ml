(* Tomasz Zakrzewski, nr indeksu: 336079, Zadanie "Odchyłka" *)
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;; 

let rec fold_tree f a t = 
	match t with 
	Leaf -> a | 
	Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r);;

(* Pomocnicza procedura do wyłuskiwania 3-go elementu krotki *)
let thrd x = 
	let (_, _, c) = x in
		c;;

(* Procedura odchylka z treści zadania. Jako akumulator przekazujemy krotkę: (maksymalna wartość poniżej danego węzła, minimalna wartość poniżej danego węzła, najlepszy wynik poniżej danego węzła *)
let odchylka t = 
	thrd (fold_tree (
		fun v (lMax, lMin, lRes) (rMax, rMin, rRes) -> 
			let minim = min lMin rMin and	(* Pomocnicze wartości, żeby skrócić trochę zapis - minimum i maksimum z wartości minimalnych i maksymalnych z poddrzew *)
			maxim = max lMax rMax in
				(max maxim v, min minim v, max (abs (maxim - v)) (abs (v - minim))) 
		)
		(0, max_int, 0) t);;