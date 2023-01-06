module type OBSLUGA_KOLEJKI =
sig
  type 'a tk
  exception Pusta of string
  val tworz_pusta: unit -> 'a tk
  val do_kolejki: 'a * 'a tk -> 'a tk
  val z_kolejki: 'a tk -> 'a tk
  val pierwszy_element: 'a tk -> 'a
  val kolejka_pusta: 'a tk -> bool
end;;


module Kolejka : OBSLUGA_KOLEJKI =
  struct
    type 'a tk= KolejkaPusta | Skladowa of 'a * 'a tk
    exception Pusta of string
    let tworz_pusta() = KolejkaPusta
    let do_kolejki(e, k) = Skladowa(e, k)
    let rec z_kolejki = function
        Skladowa(e, k) when k = KolejkaPusta -> KolejkaPusta
      | Skladowa(e, k) -> Skladowa(e, z_kolejki(k))
      | KolejkaPusta -> raise (Pusta"pusta kolejka!")
    let rec pierwszy_element = function
        Skladowa(e, k) when k = KolejkaPusta -> e
      | Skladowa (e, k) -> pierwszy_element(k)
      | KolejkaPusta -> raise (Pusta "pusta kolejka!")
    let kolejka_pusta k = k = KolejkaPusta
  end;;


let k = Kolejka.(do_kolejki(3,do_kolejki(2,do_kolejki(1,tworz_pusta()))));;
Kolejka.pierwszy_element k;;
Kolejka.(pierwszy_element(z_kolejki k));;
Kolejka.(pierwszy_element (z_kolejki (z_kolejki k)));;

Kolejka.(pierwszy_element(z_kolejki(z_kolejki (z_kolejki k))));;

Kolejka.(kolejka_pusta(z_kolejki(z_kolejki (z_kolejki k))));;

Kolejka.(kolejka_pusta k);;





(*macierz prostokatna*)

let rec summArray arr =
  if Array.length arr  = 0 then raise (Failure "pusta tablica")
  else let rec summRec x y summ result =
         match (x, y) with
           (x, y) when x = Array.length arr -> result
         | (x,y) when y = Array.length arr.(1) -> summRec (x+1) 0 0 (Array.append result (Array.make 1 summ))
         | (x,y) -> summRec x (y+1) (arr.(x).(y) + summ) result
      in summRec 0 0 0 (Array.make 0 0);;

let array1 = Array.make_matrix 3 3 1;;

Array.length (array1.(1))

array1.(1).(1);;
summArray array1;;

let arraylist = [[1;2;3;4];[5;6;7;8];[9;10;11;12]];;


summArray (Array.of_list (List.map Array.of_list [[4;2;1;4];[5;0;7;8];[9;20;5;12]]));;


let rec smallestArray arr =
  if Array.length arr  = 0 then raise (Failure "pusta tablica")
  else let rec smallestRec x y smallest result =
         match (x, y) with
           (x, y) when x = Array.length arr -> result
         | (x,y) when y = Array.length arr.(1) -> smallestRec (x+1) 0 arr.(if x+1 < (Array.length arr) then x+1 else 0).(0) (Array.append result (Array.make 1 smallest))
         | (x,y) -> smallestRec x (y+1) (if arr.(x).(y) < smallest then arr.(x).(y) else smallest) result
      in smallestRec 0 0 arr.(0).(0) (Array.make 0 0);;

smallestArray (Array.of_list (List.map Array.of_list [[4;2;1;4];[5;0;7;8];[9;20;5;12]]));;


let smallestArrayImperative arr =
  if Array.length arr = 0 then raise (Failure "pusta tablica")
  else
    let smallestArr = Array.make (Array.length arr) 0 in
    let x = Array.length arr -1 and y = Array.length arr.(1) -1 in
    for i=0 to x do
      let smallest = ref arr.(i).(0) in
      for j=0 to y do
         if arr.(i).(j) < !smallest then smallest := arr.(i).(j)
         done;
         smallestArr.(i) <- !smallest
    done;
    smallestArr;;


smallestArrayImperative array1;;
smallestArrayImperative (Array.of_list (List.map Array.of_list [[4;2;1;4];[5;0;7;8];[9;20;5;12]]));;
