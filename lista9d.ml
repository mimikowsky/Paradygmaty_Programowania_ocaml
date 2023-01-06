(*srodowa*)
(*zad1*)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lhd = function
    LNil -> failwith "lhd"
  | LCons(x, _) -> x;;

let ltl = function
    LNil -> failwith "ltl"
  | LCons(_, xf) -> xf();;

let rec lfrom k = LCons(k, function()->lfrom(k+1));;

let rec ltake (n, lxs) =
  match (n, lxs) with
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, xf)) -> x::ltake(n-1, xf());;

let rec toLazyList xs =
  match xs with
  | [] -> LNil
  | h::t -> LCons(h, function () -> toLazyList t);;

let positionsOfElem el lxs =
  let rec positionsOfElemRec elem list pos =
    match list with
    | LNil -> LNil
    | LCons(x, xf) -> if x = elem then LCons(pos, function()-> positionsOfElemRec elem (xf()) (pos+1))
                      else positionsOfElemRec elem (xf()) (pos+1)
  in positionsOfElemRec el lxs 1;;

let listaLeniwa = toLazyList([1; 2; 3; 1; 4; 1; 5]);;
ltake(4, positionsOfElem 1 listaLeniwa);;
ltake(4, positionsOfElem 8 listaLeniwa);;

(*zad2*)

type slowo = int * int;;
type slownik = slowo list;;

let s1:slowo = (5, 1);;
let s2:slowo = (4, 1);;
let s3:slowo = (3, 3);;
let s4:slowo = (2, 4);;
let s5:slowo = (1, 5);;

let slownik1:slownik = [s1; s2; s3; s4; s5];;

let usun w s =
  if s = [] then raise (Failure "pusty slownik")
  else let rec usunRec wartosc slownik =
         match slownik with
        | [] -> raise (Failure "wartosci nie ma w slowniku")
        |((war, wystapienia)::tail) -> if war = wartosc then if wystapienia>1 then((war, wystapienia-1)::tail) else tail 
                                       else (war, wystapienia)::usunRec wartosc tail
       in usunRec w s;;

usun 4 slownik1;;
usun 3 slownik1;;
usun 8 slownik1;;
usun 8 [];;
