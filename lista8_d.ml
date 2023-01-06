(* srodowa *)
(*zad1*)
type osoba = string*string;;

type drzewo_osob = Empty | Wezel of osoba * drzewo_osob * drzewo_osob;;

let czyKrotkieImie (os: osoba) =
  match os with
    |(x, y) when String.length x < 5 -> true
    |_ -> false;;

let domin: osoba = "Dominik", "Patrzek";;
let wera: osoba = "Wera", "Iksinska";;
let maxi: osoba = "Maxi", "Damn";;
let jacob: osoba = "Jacques", "Rest";;

let drzewkoosob = Wezel(domin, Wezel(wera, Empty, Empty), Wezel(maxi, Empty, Wezel(jacob, Empty, Empty)))
czyKrotkieImie domin;;

let rec filterOsoby drzewo f =
  let rec filterRec drzewo =
    match drzewo with
    |Wezel(os, d1, d2) -> if f os then (os :: filterRec d1 @ filterRec d2) else filterRec d1 @ filterRec d2
    |Empty -> []
    (*|_ -> []*)
  in filterRec drzewo;;

filterOsoby drzewkoosob czyKrotkieImie;;


(*dysk*)
(*zad3*)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let rec are_equal tree1 tree2 = 
  match (tree1, tree2) with 
  | (Empty, Empty) -> true
  | (Node (x1, tl1, tr1), Node (x2, tl2, tr2)) when x1 == x2 -> are_equal tl1 tl2 && are_equal tr1 tr2
  | _ -> false

let tree1: 'a bt = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;
let tree2: 'a bt = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;

let tree3: 'a bt = Node(2,Node(1,Empty,Node(3,Empty,Empty)),Empty);;

are_equal tree1 tree2;;
are_equal tree1 tree3;;


let rec count_subtree t1 t2 =
  match t2 with
    Empty -> 0
  | Node (v, l, r) ->
     (if t1 = t2 then 1 else 0) + (count_subtree t1 l) + (count_subtree t1 r)


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

let tt2= Node(1,Node(2,Node(3,Empty,Empty),Node(3,Empty,Empty)),Node(3, Empty, Empty));;
let tt4 = Node(3, Empty, Empty);;
let tt3= Node(2, Empty, Node(3, Empty, Empty));;


let equal tree1 tree2 =
  let rec equalNode node1 node2 =
    match (node1, node2) with
    | (Empty, Empty) -> true
    | (Empty, _) -> false
    | (_, Empty) -> false
    | (Node(value1, leftSubtree1, rightSubtree1), Node(value2, leftSubtree2, rightSubtree2)) ->
       if value1 = value2 then equalNode leftSubtree1 leftSubtree2 && equalNode rightSubtree2 rightSubtree2 else false
  in equalNode tree1 tree2;;

let equalSimple tree1 tree2 =
  tree1=tree2;;

let rec isSubtree subtree tree =
  match tree with
  | t when t = subtree -> true
  | Empty -> false
  | Node(value, leftSubtree, rightSubtree) -> if leftSubtree = subtree || rightSubtree = subtree then true
                                              else isSubtree subtree leftSubtree || isSubtree subtree rightSubtree;;

let countSubtrees subtree tree =
  let rec countSubtreesRec treeRec =
    match treeRec with
    | t when t = subtree -> 1
    | Empty -> 0
    | Node(value, leftSubtree, rightSubtree) -> if leftSubtree = subtree then 1+countSubtreesRec rightSubtree
                                                else if rightSubtree = subtree then 1+countSubtreesRec leftSubtree
                                                else countSubtreesRec leftSubtree + countSubtreesRec rightSubtree
  in countSubtreesRec tree;; 


equal tt tt2;;
equalSimple tt tt2;;

isSubtree tt2 tt;;
isSubtree tt tt;;
isSubtree tt3 tt2;;

countSubtrees tt4 tt2;;


(*let rec check_subtree tree1 tree2 =
match tree1, tree2 with
  | Empty, Empty -> true
  | Empty, Node(_, _, _) -> true
  | Node(_, _, _), Empty -> false
  | Node(x1, l1, r1), Node(x2, l2, r2) -> 
    if x1 <> x2 then false 
    else (check_subtree l1 l2) && (check_subtree r1 r2);;
 *)
