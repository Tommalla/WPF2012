(* Tomasz Zakrzewski, Praca Domowa I *)
(* Procedura z treści zadania *)
let zera a b =
  (* Pomocnicza procedura licząca ilość zer w zapisie dziesiętnym danej liczby x *)
  let rec countZeros x acc =
    if x <= 0 
    then
      acc
    else
      countZeros (x/10) (acc+(if (x mod 10)=0 then 1 else 0 ) )
  in
    (* Faktyczna rekurencja licząca wynik. n - aktualna sprawdzana liczba *)
    let rec helper n res =
      if n > b 
      then
        res
      else
        helper (n+1) (res + (countZeros n 0))
    in helper a 0;;