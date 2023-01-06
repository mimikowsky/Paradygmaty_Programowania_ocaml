(*srodowe zajecia*)

type litera = Tak | Nie;;

type dane_slow = Puste | Krotkie of litera| Srednie of litera | Dlugie of litera ;;
  
type dlugosc = Puste | Krotkie | Srednie | Dlugie;;


let rec infFunction lista znak =
  match lista with
  | (hd::tl) when  String.length hd <= 0  -> Puste :: infFunction tl znak
  | (hd::tl) when  String.length hd <= 10 -> Krotkie(if String.contains hd znak then Tak else Nie) :: infFunction tl znak
  | (hd::tl) when  String.length hd <= 20 -> Srednie(if String.contains hd znak then Tak else Nie) :: infFunction tl znak
  | (hd::tl) when  String.length hd > 20 -> Dlugie(if String.contains hd znak then Tak else Nie) :: infFunction tl znak
  | [] -> []
  | (hd::tl) -> Puste :: infFunction tl znak;;


infFunction ["abc"; ""; "dlugieeeeslowooooooooo"; "sredniebedzie"] 'e';;
(* dysk *)

(* lista 6 dysk *)

(*zad 1 *)
type osoba = string * string;;

type plec = Kobieta of osoba | Mezczyzna of osoba;;

let (osoba1: osoba) = ("Dominik","Patrzek");;

let okreslPlec (os: osoba) =
   match os with 
   | (x, y) when (String.lowercase x).[(String.length x) - 1 ] == 'a' -> Kobieta(x,y)
   | _ -> Mezczyzna ( fst os, snd os);;

okreslPlec osoba1;;


let k1 = Kobieta ("xx", "yy");;
okreslPlec k1;;

(* second version*)

type plec = Mezczyzna | Kobieta;;
type osoba = string*string;;
let osoba1:osoba = ("Srodka", "Krzyszta");;

let sprawdz o =
  let imie = String.lowercase (snd(o:osoba))
  in if imie.[(String.length imie-1)] == 'a' then Kobieta else Mezczyzna;;


sprawdz osoba1;;


(* zad 2 *)

type samochod = string*string*int;;
type samochody = samochod list;;

let s1:samochod = ("Opel", "astra", 2002);;
let s2:samochod = ("Renault", "megane", 2002);;
let s3:samochod = ("Toyota", "avensis", 2009);;
let s4:samochod =  ("Nissan", "micra", 2003);;
let listaSamochodow:samochody = [s1; s2; s3; s4];;

let autaMinRocznik listaAut = 
  let minRocznik =
    List.fold_left(fun min (s:samochod) -> match s with
                                          | (_, _, rocznik) -> if rocznik < min then rocznik else min)3000 listaAut in
List.filter(fun (s:samochod)->(match s with (_,_,x)->x)==minRocznik) listaAut;;

autaMinRocznik listaSamochodow;;

(* List.fold_left(fun min (s:samochod  [ min to akumulator, a s to kolejny element listy ]*)
