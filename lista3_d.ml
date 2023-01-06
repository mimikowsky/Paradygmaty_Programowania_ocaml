let eulerFunc n =
  if n=0. then raise (Failure"zly n!")
  else let rec eulerRec(i, summ) =
         if i = n then summ -. log(i)
         else eulerRec(i+.1., summ+.(1./.i))
         in eulerRec(1., 0.);;

eulerFunc(500.);;

let eulerFuncP precision =
  if precision <=0. then raise (Failure"zla dokladnosc!")
  else let rec eulerRec(i, summ, oldValue, value) =
         if ( if oldValue-.value > 0. then oldValue-.value else value-.oldValue) < precision then value
         else eulerRec(i+.1., summ+.(1./.i), value, summ +.(1./.i) - log(i))
         in eulerRec(1., 0., 0., 0.);;

eulerFuncP(0.00003);;



let rec removeFunc n lista = 
        match (n, lista) with
        | (0, hd :: tl) -> hd :: tl
        | (_, [] )      -> [] 
        | (x, hd :: tl) -> removeFunc (x-1) (List.rev(List.tl(List.rev tl)));;

removeFunc(1) (1::3::5::[]);;
let x = (false, 10);;
let (false, y) = x;;

let rec badAppend l1 l2 =
match (l1, l2) with
([], []) -> []
| ([], h2::t2) -> h2:: badAppend [] t2
| (h1 :: t1, []) -> h1 :: badAppend t1 []
| (h1 :: t1, h2::t2) -> h1 :: badAppend t1 l2;;





let rec func2 list =
  if list = [] then raise (Failure"pusta lista")
  else let rec funcRecPow n lista =
         match (n, lista) with
         | (_, []) -> []
         | (0.0, hd :: tl) -> hd :: funcRecPow (n+.1.) tl
         | (i, hd :: tl) -> hd *. hd :: funcRecPow (i-.1.) (hd :: tl)
       in funcRecPow(1.0, list);;



let rec func2 list =
  if list = [] then raise (Failure"pusta lista")
  else let rec funcRecPow n lista listaWynikowa =
         match (n, lista) with
         | (_, []) -> listaWynikowa
         | (0, hd :: tl) -> funcRecPow (n+1) tl (hd::listaWynikowa)
         | (i, hd :: tl) -> funcRecPow (i-1) (hd :: tl) ((hd *. hd) :: listaWynikowa)
       in funcRecPow(1, list, []);;



let rec func2 list =
         match list with
         |  [] -> []
         | (0, hd : tl) -> hd *. hd :: funcRecPow(n+1, tl)
         | (i, hd :: tl) -> hd *. hd :: funcRecPow(i-1, tl)


let rec sqr_list l =
match l with
[] -> []
| h::t -> h*h :: sqr_list t;;
| 

sqr_list (1::3::5::7::[])






let eulerConst prec =
  let rec eulerRec (i, pres, next,sum) =
    let pres = sum+.1./.i-.log(i) in
    let next = sum+.1./.(i+.1.)-.log(i+.1.) in
    if abs_float(pres-.next)<=prec then next
    else eulerRec(i+.1., pres, next, sum+.1./.i)
  in eulerRec(1., 0., 0., 0.);;

eulerConst(0.03);;

let plus (x,y) = x+y;;
let add x y = x + y;;
let f1 x = x 2 2;;

let ((n,w) as d, b) = x;;
l
et curry f x y = f (x, y);;
