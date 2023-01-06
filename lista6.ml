(* zad 1 *)

type osoba = string * string;;

type plec = Kobieta of osoba | Mezczyzna of osoba;;

let okreslPlec (os: osoba) =
   match os with 
   | (x, y) when (String.lowercase x).[(String.length x) - 1 ] == 'a' -> Kobieta(x,y)
   | _ -> Mezczyzna ( fst os, snd os);;


let (osoba1: osoba) = ("Dominik","Patrzek");;
let (osoba2: osoba) = ("Anna","Wozny");;

okreslPlec osoba1;;
okreslPlec osoba2;;

(* zad 2 *)

type samochod = string*string*float;;
type samochody = samochod list;;

let s1:samochod = ("Opel", "astra", 1999.);;
let s2:samochod = ("Renault", "megane", 2004.);;
let s3:samochod = ("Toyota", "avensis", 2009.);;
let s4:samochod =  ("Nissan", "micra", 2003.);;

let listaSamochodow:samochody = [s1; s2; s3; s4];;

let autaSrednia listaAut = 
  let suma =
    List.fold_left(fun suma (s:samochod) -> match s with
                                          | (_, _, rocznik) -> suma +. rocznik) 0. listaAut in
    suma /. float(List.length listaAut);;

autaSrednia listaSamochodow;;


(* zad 3 *)

type 'a wybor = Brak | Wartosc of 'a;;

let xx: 'a wybor = Brak;;
let xy: 'a wybor = Wartosc 5;;

type 'a drzewo = Lisc of 'a | Wezelx of 'a drzewo * 'a drzewo | Wezel of 'a drzewo

let treee: 'a drzewo = Wezelx(Lisc 1, Wezelx(Lisc 2, Wezelx(Lisc 4, Lisc 3)));;
let treee1: 'a drzewo = Wezelx(Wezelx(Lisc 7, Lisc 8), Wezelx(Lisc 2, Wezelx(Lisc 4, Lisc 3)));;


let rec wyborFunc tree =
  let rec wyborFuncRec tree =
    match tree with
   Lisc (x) :: tl  -> Wartosc x :: (wyborFuncRec tl)
  | Wezelx (left, right) :: tl -> (wyborFuncRec [left]) @ wyborFuncRec [right]@ wyborFuncRec tl @ Brak :: []
  | Wezel (one) :: tl -> wyborFuncRec [one] @ wyborFuncRec tl @ Brak :: []
  | [] -> []
  in wyborFuncRec [tree];;


wyborFunc treee;;
wyborFunc treee1;;

let treee2: 'a drzewo = Wezelx(Wezel(Lisc 5), Wezelx(Lisc 1, Wezelx(Lisc 2, Wezel(Lisc 3))));;
wyborFunc treee2;;
