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
Kolejka.pierwszy_element (Kolejka.z_kolejki k);;
Kolejka.pierwszy_element (Kolejka.z_kolejki (Kolejka.z_kolejki k));;

Kolejka.(pierwszy_element (z_kolejki (z_kolejki k)));;
Kolejka.(pierwszy_element (z_kolejki(z_kolejki (z_kolejki k))));;


let rec summArray arr =
  if Array.length arr  = 0 then raise (Failure "pusta tablica")
  else let rec summRec x y summ result =
         match (x, y) with
           (x, y) when x = Array.length arr (*&&  y = Array.length arr.(1)*) -> result
         | (x,y) when y = Array.length arr.(1) -> summRec (x+1) 0 0 (Array.append result (Array.make 1 summ))
         | (x,y) -> summRec x (y+1) (arr.(x).(y) + summ) result
      in summRec 0 0 0 (Array.make 0 0);;

let array1 = Array.make_matrix 3 3 1;;
Array.length (array1.(1))
  array1.(1).(1);;
summArray array1;;
summArray two_d_array;;




let two_d_array =
  Array.make_matrix 3 3 0
  |> Array.mapi (fun i row ->
    Array.mapi (fun j col -> i * 3 + j + 1) row
  )


(* Funkcyjny:*)
(*let rec sum_of_row matrix = 
  match matrix with 
  | [] -> []
  | h::t -> let row_sum = List.fold_left (+) 0 h in 
            row_sum :: sum_of_row t
;;
 *)


(* Imperatywny:*)
(*let sum_of_row matrix = 
  let row_sums = ref [] in 
  let row_length = Array.length matrix.(0) in 
  for i = 0 to (Array.length matrix - 1) do
    let row_sum = ref 0 in 
    for j = 0 to (row_length - 1) do
      row_sum := !row_sum + matrix.(i).(j)
    done;
    row_sums := !row_sums @ [!row_sum]
  done;
  Array.of_list !row_sums
;;
 *)
