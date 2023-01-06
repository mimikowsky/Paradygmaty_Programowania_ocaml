let functionA(a1, a2, number) =
  (a1 :: a2 :: []), number*.2.



let functionB(lista1, lista2) =
  if(lista1 = [] || lista2 = []) then raise (Failure"pusta lista!")
  else if ((List.hd(List.hd lista1))*.2.  == (List.hd(List.hd lista2))*.2. ) then true
  else false



let functionC(lista1, lista2, b) =
  List.hd lista1 :: List.hd lista2 :: [], (List.hd lista1, List.hd lista2)



let function3(lista, number) =
  if lista = [] then raise(Failure "pusta lista")
  else if(number<1) then raise (Failure "zly n!")
  else let rec funRec(lista1, counter, listaWynikowa, pomocnicza) =
         if lista1 = [] then  List.rev(List.rev(pomocnicza)::listaWynikowa)
         else if(counter == 0) then funRec(lista1, number,List.rev(pomocnicza)::listaWynikowa, [])
         else funRec(List.tl lista1, counter-1, listaWynikowa, List.hd lista1::pomocnicza)
         in funRec(lista, number, [], []);;

function3(1 :: 3 :: 5 :: 8 :: 4 :: 6 :: 7 :: 8 :: 10 :: [], 2);;
function3([], 2);;
function3(1 :: 3 :: 5 :: 8 :: 4 :: 6 :: 7 :: 8 :: 10 :: [], 0);;
function3(1 :: 3 :: 5 :: 8 :: 4 :: 6 :: 7 :: 8 :: 10 :: [], 1);;




























let function3x(lista, number) =
  if lista = [] then raise(Failure "pusta lista")
  else if(number<1) then raise (Failure "ujemny n!")
  else let rec funRec(lista1, counter, listaWynikowa, pomocnicza) =
         if lista1 = [] then listaWynikowa@pomocnicza
         else if(counter == 0) then funRec(lista1, number, listaWynikowa::pomocnicza, [])
         else funRec(List.tl lista1, counter-1, listaWynikowa, pomocnicza @ [List.hd lista1])
         in funRec(lista, number, [], []);;

function3x(1 :: 3 :: 5 :: 8 :: 4 :: 6 :: 7 :: 8 :: 10 :: [], 2);;




let split (input, x) =
  if x<=0 then raise (Failure "ujemny argument")
  else if input = [] then [[]]
  else let rec makeList (list, result,partial, i)=
         if list = [] then partial::result)
         else if i=x then makeList(list, partial::result,[],0) 
         else makeList(List.tl list, result, List.hd list::partial,i+1)
       in makeList(input,[],[],0);;

split(1 :: 3 :: 5 :: 8 :: 4 :: 6 :: 7 :: 8 :: [], 2);;
