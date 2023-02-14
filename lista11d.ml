type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk

module type X =
  sig
  val x : int
  end;;


module Para = functor (El : sig type t end) ->
struct type para = El.t * El.t end;;


module Para(El : sig type t end) =
  struct type para = El.t * El.t end;;


module type Y =
   sig type t end


module Para(El : Y) =
  struct type para = El.t * El.t end;;


type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk

module KolejkaFunktor (T : sig type t end) = struct
  type element = T.t
  type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk

  let tworz_kolejke () = KolejkaPusta

  let wstaw element kolejka = Skladowa (element, kolejka)

  let wybierz_pierwszy = function
    | KolejkaPusta -> None
    | Skladowa (x, _) -> Some x

  let usun_pierwszy = function
    | KolejkaPusta -> KolejkaPusta
    | Skladowa (_, xs) -> xs

  let czy_pusta = function
    | KolejkaPusta -> true
    | _ -> false

  let wyswietl_lista =
    let rec wyswietl_lista_aux kolejka acc =
      match kolejka with
      | KolejkaPusta -> acc
      | Skladowa (x, xs) -> wyswietl_lista_aux xs (x :: acc)
    in
    wyswietl_lista_aux

  let wyswietl_lista_par =
    let rec wyswietl_lista_par_aux kolejka acc pozycja =
      match kolejka with
      | KolejkaPusta -> acc
      | Skladowa (x, xs) ->
        wyswietl_lista_par_aux xs ((x, pozycja) :: acc) (pozycja + 1)
    in
    wyswietl_lista_par_aux
end

(* Przyk�ad dzia�ania dla typu prostego *)
module KolejkaInt = KolejkaFunktor (struct type t = int end)

let kolejka = KolejkaInt.tworz_kolejke ()
let kolejka = KolejkaInt.wstaw 1 kolejka
let kolejka = KolejkaInt.wstaw 2 kolejka
let kolejka = KolejkaInt.wstaw 3 kolejka

let pierwszy_element = KolejkaInt.wybierz_pierwszy kolejka
let _ = assert (pierwszy_element = Some 1)

let kolejka = KolejkaInt.usun_pierwszy kolejka
let pierwszy_element = KolejkaInt.wybierz_pierwszy kolejka
let _ = assert (pierwszy_element = Some 2)

let czy_pusta = KolejkaInt.czy_pusta kolejka
let _ = assert (czy_pusta = false)

let lista = KolejkaInt.wyswietl_lista kolejka []
let _ = assert (lista = [2; 3])

let lista_par = KolejkaInt.wyswietl_lista_par kolejka [] 0
let _ = assert (lista_par = [(2, 0); (3, 1)])

(* Przyk�ad dzia�ania dla typu z�o�onego *)
module KolejkaStringList = KolejkaFunktor (struct type t = string list end)

let kolejka = KolejkaStringList.tworz_kolejke ()
let kolejka = KolejkaStringList.wstaw ["a"; "b"] kolejka
let kolejka = KolejkaStringList.wstaw ["c"; "d"] kolejka
let kolejka = KolejkaStringList.wstaw ["e"; "f"] kolejka

let pierwszy_element = KolejkaStringList.wybierz_pierwszy kolejka

