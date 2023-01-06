
let pole a b c =
  if( a < 0. || b < 0. || c < 0.) then raise (Failure "ujemny argument")
  else if abs_float(b-.c) >=  a ||  a >= b+.c then raise (Failure "zle boki")
else
  let p =( a +. b +. c)/.2.
  in sqrt(p*.(p-.a)*.(p-.b)*.(p-.c));;

pole 3. 4. 5.;;

(*
let rec suma n x wynik  =
  if n < 0  then raise (Failure "ujemny argument")
  if n == 0 then wynik
  else
    wynik += wynik*(-1)*x
    suma(n-1)
    in suma()
 *)

let suma(x, n)=
  if n < 1  then raise (Failure "zly n!")
   else let rec suma1(wynik,i)=
      if (i == n) then wynik*.(-1.)*.x/.i.
      else  wynik*.(-1.)*.x/.i +. suma1(wynik, i+1)
        in suma1(1., 1.);;

suma(5, 1);;


(*
let rec potegi (x, n) =
  if (n < 0) then raise (Failure "ujemne n!")
  else if (n == 0) then 1
  else 
 *)


(* zad3*)

let smallestNumber lista =
  if lista == [] then raise (Failure "Pusta lista!")
  else let rec smallestRec(smallest, lista1) =
      if (List.length lista1 == 1) then smallest
      else  if (smallest < List.hd(List.tl lista1)) then smallestRec(smallest, List.tl lista1)
      else smallestRec(List.hd(List.tl lista1), List.tl lista1)
      in smallestRec(List.hd lista, lista);;


smallestNumber(-10 :: -5 :: 1 :: -20 :: []);;


let splitList(list) =
  if list = [] then raise (Failure "Pusta lista!")
  else let rec splitListRec(list1, listNeg, listPlus) =
         if list1 = [] then (listNeg :: listPlus :: [])
         else if(List.hd list1 > 0) then splitListRec(List.tl list1, listNeg, listPlus @ [List.hd list1])
         else splitListRec(List.tl list1, listNeg @ [List.hd list1], listPlus)
       in splitListRec(list, [], []);;

splitList(1 :: 3 :: -2 :: 5 :: -1 :: [])
