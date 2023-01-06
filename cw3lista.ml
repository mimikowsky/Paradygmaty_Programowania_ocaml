(* zad 1*)

let f1 x = x 2 2;;
(* val f1 : (int -> int -> 'a) -> 'a = <fun> *)

(* jako x podajemy funkcje ktora wywola sie dla dwoch argumentow o wartosci 2 2*)

let f2 x y z = x ( y ^z );;
(* val f2 : (string -> 'a) -> string -> string -> 'a = <fun> *)

(* jako x podajemy funkcje ktora bierze stringa i daje wynik, konkatenacja y i z to bedzie argument naszej funkcji, np taka*)

let rec func2 str = String.length str
f2 func2 "nie" "tak";;


(*zad 2 *)

(* rozwijamy funkcje f(x, y, z) i dzieki temu wywolujac curry podajemy bez nawiasow*)
let curry3 f x y z = f(x, y, z);;

let funcTocurry (x, y, z) = x+y+z;;
funcTocurry (1, 2, 3);;
curry3 funcTocurry 1 2 3;;

let curry3BezLukru = function f -> function x -> function y -> function z -> f(x,y,z);;

curry3:  ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd

(*teraz uncurry, zwinieta funkcja, mozemy wywolac ta funkcje podajac argumenty w nawiasach*)
let uncurry3 f(x, y, z) = f x y z;;

let uncurry3BezLukru = function f -> function(x,y,z) -> f x y z;;

uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd

(* zad 3*)

let rec sumProd xs =
  match xs with
    h::t -> let (s,p)= sumProd t
            in (h+s,h*p)
  | [] -> (0,1);;

sumProd [1;2;3;4];;

List.fold_left;;

let sumProdFold xs =
  List.fold_left(fun (s, p) x -> (s+x, p*x))(0, 1) xs;;

sumProdFold [1;2;3;4];;

(* zad 4 *)

let rec quicksort = function
 [] -> []
 | [x] -> [x]
 | xs -> let small = List.filter (fun y -> y < List.hd xs ) xs
 and large = List.filter (fun y -> y >= List.hd xs ) xs
 in quicksort small @ quicksort large;;


let rec quicksort' = function
 [] -> []
 | x::xs -> let small = List.filter (fun y -> y < x ) xs
 and large = List.filter (fun y -> y > x ) xs
 in quicksort' small @ (x :: quicksort' large);;
