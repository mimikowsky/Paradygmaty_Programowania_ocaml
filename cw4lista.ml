(* zad 1*)
let f1 x y z = x y z;;
(*val f1 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>*)

let f2 x y = function z -> x::y;;
(*val f2 : 'a -> 'a list -> 'b -> 'a list = <fun>*)

f2 "s" ["adas"; "sen"] 5

let f x = x+5

(* zad 2 *)

(* dowolna funkcja  f: 'a -> 'b *)
let rec f x = f x;;
let a x = List.hd [];;
let e x = raise Not_found;;

(*Jedynym sposobem na spełnienie stwierdzenia „dla wszystkich typów b zwracana wartość jest wartością typu b”
 jest upewnienie się, że funkcja nie zwraca.*)
(* Dwie możliwości: albo błąd, albo zapetlenie się. Funkcja zgłasza błędy, albo nie kończy się *)

(* zad 3 *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt

let tt = Node(1,
              Node(2,
                   Node(4,
                        Empty,
                        Empty
                     ),
                   Empty
                ),
              Node(3,
                   Node(5,
                        Empty,
                        Node(6,
                             Empty,
                             Empty
                          )
                     ),
                   Empty
                )
           );;

(*let rec breadthBT tree =
   match tree with
     Empty -> []
   | Empty :: tail -> breadthBT tail
   | Node(value, left, right) :: tail -> value ::);;
*)

(*
let breaddthBT tree =
  let rec helper = function
	[] -> []
	| (Empty :: tail) -> helper tail
	| (Node(value, leftSubtree, rightSubtree) :: tail) -> value :: helper (leftSubtree :: rightSubtree :: tail)
	in helper [tree];;

breaddthBT tt;;
 *)


(* zad 5*)

type 'a graph = Graph of ('a -> 'a list);;

let g = Graph
 (function
  0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;


let breadthSearch (Graph succ) startNode =
let rec search visited queue =
match queue with
[] -> []
| h::t -> if List.mem h visited then search visited t
          else h :: search (h :: visited) (t @ succ h)
in search [] [startNode];;

breadthSearch g 4;;


let depthSearch (Graph succ) startNode =
let rec search visited queue =
match queue with
[] -> []
| h::t -> if List.mem h visited then search visited t
          else h :: search (h :: visited) (succ h @ t)
in search [] [startNode];;

depthSearch g 4;;
