(* zad 3 *)

type 'a drzewo = Lisc of 'a | Wezel of 'a drzewo * 'a drzewo;;

let tree: 'a drzewo = Wezel(Wezel(Lisc 1, Lisc 2), Wezel(Lisc 3, Lisc 4));;
let tree1: 'a drzewo = Wezel(Wezel(Lisc 1, Lisc 2), Wezel(Lisc 3, Lisc 4));;
let subTree: 'a drzewo = Wezel(Lisc 1, Lisc 2);;

let rec isSubTree treeA treeB =
  let rec isSubTreeRec sub tre =
    match (sub, tre) with
      (Lisc x :: tlx, Lisc y :: tly) when x = y -> isSubTreeRec tlx tly
    | (Lisc x :: [], Lisc y ::[]) when x=y -> true
    | (Wezel(leftx, rightx)::tlx, Wezel(lefty,righty)::tly) -> isSubTreeRec [leftx] [lefty] || isSubTreeRec [rightx] [righty] ||  isSubTreeRec tlx tly
    | (Wezel(leftx, rightx)::[], Wezel(lefty,righty)::[]) -> isSubTreeRec [leftx] [lefty] || isSubTreeRec [rightx] [righty]
    | (_,_) -> false
  in isSubTreeRec [treeA] [treeB];;


isSubTree subTree tree;;


let rec is_subtree t1 t2 =
  match t1, t2 with
  | Lisc v1, Lisc v2 -> v1 = v2
  | Wezel (l1, r1), Wezel (l2, r2) -> (is_subtree l1 l2) || (is_subtree r1 r2)
  | _ -> false

is_subtree tree tree1;;

let rec is_subtree tree1 tree2 = 
  match (tree1, tree2) with 
  | (Lisc v1, Lisc v2) -> v1 = v2
  | (Wezel (tl1, tr1), Wezel (tl2, tr2)) -> (is_subtree tl1 tl2 || is_subtree tr1 tr2)
  | (_, Lisc _) -> false
  | (Lisc _, _) -> false

is_subtree subTree tree;;
