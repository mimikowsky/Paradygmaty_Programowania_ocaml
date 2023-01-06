let silnia' n =
let rec f(i,s) = if i<n then f(i+1,(i+1)*s) else (i,s)
in snd (f(0,1));;
(*snd znaczy chyba ze zwraca w koncowym etapie drugi element tylko*)

silnia' 5;;

List.fold_left;;
(* - : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>  *)
(*   *)

List.fold_right;;


let funkcjaY x =
  2.*.x +. 5.;;

funkcjaY 5.;;

let pole f a b dx =
  fold_left f (a+.b) 0.



let rec max_list smallest lst = match lst with 
    | []   -> smallest
    | h::t -> max_list (max smallest h) t

max_list 4 [5;3;1;8;2];;

let max_list smallest lst =
    List.fold_left (fun acc x -> max acc x) smallest lst

let bin2dec lb =
    List.fold_left (fun acc b -> acc*2+b) 0 lb;;

bin2dec [1;1;1;0];;
bin2dec [];;
bin2dec [1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];; //2^32 -1
