let rec func2 list =
  if list = [] then raise (Failure"pusta lista")
  else let rec funcRecPow n potega lista listaWynikowa =
         match (n, lista) with
         | (_, []) -> List.rev listaWynikowa
         | (0, hd :: tl) -> funcRecPow (potega) (potega+1) tl listaWynikowa
         | (i, hd :: tl) when i=(potega-1) -> funcRecPow (i-1) potega (hd :: tl) (hd :: listaWynikowa)
         | (i, hd :: tl) ->  funcRecPow (i-1) potega (hd :: tl) ((hd *. List.hd listaWynikowa) :: listaWynikowa)
       in funcRecPow 1 2 list [];;


func2 [1.;2.;5.;10.];;

 
  
