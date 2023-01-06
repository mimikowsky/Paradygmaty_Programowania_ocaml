
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec toLazyList = function
 [] -> LNil
  | x :: xs -> LCons(x, lazy (toLazyList xs));;


let rec ltake = function
 (0, _) -> []
 | (_, LNil) -> []
 | (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs);;


let rec lzip (lxs, lys) =
 match (lxs, lys) with
 (LCons(h1, lazy t1), LCons(h2, lazy t2)) -> LCons((h1, h2), lazy (lzip (t1, t2)))
 | _ -> LNil;;

(* val lzip : 'a llist * 'b llist -> ('a * 'b) llist = <fun> *)

let plxs = lzip(lfrom 1, lfrom 10);;

ltake(5, plxs);;

ltake(100, lzip(lfrom 1, toLazyList ['a'; 'b'; 'c']));;


let rec lunzip plxs =
 match plxs with
 | LCons((h1, h2), lazy t) -> (LCons(h1, lazy (fst(lunzip t))), LCons(h2,lazy (snd(lunzip t))))
 | LNil -> (LNil, LNil);;
(*fst i snd because function returns 'a list and 'b list so we need in one case first and in second case second (which is a lazy list)*)
(* val lunzip : ('a * 'b) llist -> 'a llist * 'b llist = <fun> *)

let (lxs1, lxs2) = lunzip plxs;;

(ltake(5, lxs1),ltake(8 ,lxs2));;

(*zad1*)
let rec lrepeat k lazylist =
  if k <= 0 then LNil
  else let rec repeatRec n list =
    match (n, list) with
    |(1, LCons(h1, lazy tl)) -> (LCons(h1, lazy (repeatRec k tl)))
    |(n, LCons(h1, lazy tl)) -> LCons(h1, lazy (repeatRec (n-1) (LCons(h1, lazy tl))))
    |(_, LNil) -> LNil
    in repeatRec k lazylist;;


ltake (15, (lrepeat 3 (lfrom 1)));;
ltake (15, (lrepeat 0 (lfrom 1)));;

(*zad2*)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec ltake = function
 (0, _) -> []
 | (_, LNil) -> []
 | (n, LCons(x, xs )) -> x :: ltake(n-1, xs());;

let lfib =
  let rec fibgen a b =
      LCons(a, fun () -> fibgen b (a + b))
  in fibgen 0 1;;


let rec lfib =
  let rec fibgen a b =
      LCons(a, lazy (fibgen b (a + b)))
  in fibgen 0 1;;

ltake(10, lfib);;

(*zad3*)

type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

let rec lgbt k = LNode(k, (function () -> lgbt (k*2)), (function () -> lgbt (k*3)));;

(* a) *)
let lBreadth tree =
	let rec helper = function
		[] -> LNil
		| LEmpty :: tail -> helper tail
		| LNode(value, leftSubtree, rightSubtree) :: tail -> LCons(value, lazy (helper (tail @ [leftSubtree(); rightSubtree()])))
	in helper [tree];;
(*lista jest wygodna, bo dodajemy do LCons value, a potem w funkcji w pierwszej kolejnosci przetwarzamy tail czyli rownolegly poziomowo wezel, a potem dopiero jego dzieci(leftSubtree() i right zwraca po prostu drzewo)*)

ltake(10, lBreadth (lTree 1));;


let lBreadth tree =
	let rec helper = function
		(LEmpty) -> LNil
	      (*| (LNode(v, l, r)) -> helper LNode(v, l, r)*)
		| (LNode(value, leftSubtree, rightSubtree)) -> LCons(value, lazy(helper(leftSubtree();helper(rightSubtree()))))
	in helper tree;;


(* b) *)

let rec lTree n =
	LNode(n, (function () -> lTree (2 * n)), function () -> lTree (2 * n + 1));;

let x = lTree 1;;



