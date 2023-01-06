type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;



let tree1: 'a bt = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;
let tree2: 'a bt = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty);;

let tree3: 'a bt = Node(2,Node(1,Empty,Node(3,Empty,Empty)),Empty);;


let rec are_equal tree1 tree2 =
  tree1 = tree2;;

are_equal tree1 tree2;;
are_equal tree1 tree3;;

let rec is_subtree subtree tree =
  match tree with
    tree when tree == subtree -> true
   |Empty -> false
   | Node(value, leftSubtree, rightSubtree) -> if leftSubtree = subtree || rightSubtree = subtree then true
                                              else is_subtree subtree leftSubtree || is_subtree subtree rightSubtree;;


let countSubtrees subtree tree =
  let rec countSubtreesRec treeRec =
    match treeRec with
    | t when t = subtree -> 1
    | Empty -> 0
    | Node(value, leftSubtree, rightSubtree) -> if leftSubtree = subtree then 1+countSubtreesRec rightSubtree
                                                else if rightSubtree = subtree then 1+countSubtreesRec leftSubtree
                                                else countSubtreesRec leftSubtree + countSubtreesRec rightSubtree
  in countSubtreesRec tree;; 



let tt = Node(1,Node(2,Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;

let tt2= Node(1,Node(2,Node(3,Empty,Empty),Node(3,Empty,Empty)),Node(3, Empty, Empty));;
let tt3= Node(2, Empty, Node(3, Empty, Empty));;
let tt4 = Node(3, Empty, Empty);;
                              

is_subtree tt2 tt;;
is_subtree tt tt;;
is_subtree tt3 tt2;;

countSubtrees tt4 tt2;;










(*
let rec count_subtree subtree tree =
  let rec count_subtreeRec tree =
  match tree with
    tree when tree == subtree -> 1
   | Empty -> 0
   | Node(value, leftSubtree, rightSubtree) ->  if leftSubtree == subtree then 1 + count_subtreeRec rightSubtree
                                                else if rightSubtree == subtree  then 1 + count_subtreeRec leftSubtree 
                                                else count_subtreeRec leftSubtree + count_subtreeRec rightSubtree
  in count_subtreeRec subtree tree;;*)
