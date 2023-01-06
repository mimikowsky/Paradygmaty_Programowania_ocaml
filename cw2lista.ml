(*zad 1
dla f evenR(3) glebokosc stosu w Ocamlu to 1,
Ocaml optymalizuje rekurencje
*)

(*zad2 *)

let rec fib n =
  if (n < 0) then raise (Failure"ujemny n")
  else if n == 0 then 0
  else if n == 1 then 1
  else fib(n-1) + fib(n-2);;

fib 8;;
fib 42;;


let rec fib1 n =
  if (n < 0) then raise (Failure"ujemny n")
  else match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib1(n-2) + fib1(n-1);;


(*dodajemy kolejne liczby i wykonujemy to n razy, nie wracamy z rekurencji jak wczesniej*)
let fibTail n =
 if (n < 0) then raise (Failure "ujemny n")
 else let rec fHelper(n, f1, f2) = 
  match n with
  | 0 -> f1
  | 1 -> f2
  | _ -> fHelper((n - 1), f2, (f1 + f2))
in fHelper(n, 0, 1);;

fibTail 8;;
fibTail 42;;


(* zad 3 - pierwiastek stopnia 3 z a*)

let root3 a =
  let rec root3Rec x =
    if abs_float(x**3. -. a) <= 1e-15 *. abs_float(a) then x
    else root3Rec (x +. (a/.(x**2.) -. x)/.3.)
  in root3Rec(if a<=1. then a else a/.3.);;

root3 27.;;
(* zad 4 - wzorzec*)


let [_; _; x; _; _] = [-2; -1; 0; 1; 2];;

let [(_,_);(x,_)] = [(1, 2); (0, 1)];;


(* zad 5 - czy podlista listy*)

let rec initSegment (xs, ys) =
  match (xs, ys) with
  | ([], _) -> true
  | (_, []) -> false
  | (head1 :: tail1, head2 :: tail2) -> if head1 == head2 then initSegment(tail1, tail2) else false;;

initSegment([1; 2], [1; 2; 3]);;
initSegment([1; 3], [1; 2; 3]);;


(* zad 6 - zastepowanie n-ty element listy podana wartoscia*)

let rec replaceNth (list, n, el) =
  match (list, n) with
  | (_, x) when x<0 -> raise (Failure"ujemny n")
  | ([], _) -> []
  | (hd::tl, 0) -> el :: tl
  | (hd::tl, x) -> hd :: replaceNth ( tl, (x-1), el);;


replaceNth(['o';'l';'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], 1, 's');;
replaceNth([], -1, 's');;
