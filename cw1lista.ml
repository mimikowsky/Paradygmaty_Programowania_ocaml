(* zad1 *)
let rec flatten1 lista =
  if lista = [] then []
  else List.hd lista @ flatten1(List.tl lista);;

let x1 = 1. :: 2. :: 4. :: []
let x2 = 3. :: 5. :: 7. :: []
let x3 = x1 :: x2 :: [];;

flatten1 x3;;

(* zad2  *)

let rec count (x, lista) =
  if lista = [] then 0
  else if (List.hd lista == x) then 1 + count(x, List.tl lista)
  else 0 + count(x, List.tl lista);;

let x4 = 'a' :: 'l' :: 'a' :: [];;

count('a', x4);;

(* zad3  *)

let rec replicate(x, n) =
  if ( n < 0) then raise (Failure "Ujemna liczba n!")
  else if(n == 0) then []
  else x :: replicate(x, n-1);;

replicate("la", 3);;
replicate("la", -2);;

(* zad4  *)

let rec sqrList lista =
  if (lista == []) then []
  else List.hd lista * List.hd lista :: sqrList(List.tl lista);;

sqrList(1 :: 3 :: -5 :: 9 :: []);;

(* zad5  *)

let palindrome lista = 
  lista = List.rev lista;;

palindrome('a' :: 'l' :: 'a' :: []);;
palindrome('d' :: 'l' :: 'a' :: []);;

(* zad6  *)

let rec listLength lista =
  if lista == [] then 0
  else 1 + listLength(List.tl lista);;

listLength(1 :: 3 :: -5 :: 9 :: []);;
listLength('a' :: 'b' :: 'c' :: 'd' :: []);;
listLength([]);;
