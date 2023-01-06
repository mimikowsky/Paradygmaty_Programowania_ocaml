(* obliczenie pola pod krzywa metoda pojedynczych trapezow *)


let poleFunction func a b dx =

  if (a >= b) then raise (Failure"zle a i b")
  else if (dx <=0.) then raise (Failure "ujmeny dx")
  else if ( abs_float(a-.b) < dx) then raise (Failure" zly dx")
  else let rec createList a b dx =
    if(a > b) then []
    else (a, a+.dx) :: createList(a +. dx) b dx in
       
    List.fold_right (fun (x, y) acc -> ((abs_float(func x) +. abs_float(func y)) *. (y -.x)/.2.) +. acc ) (createList a b dx) 0.;;




let functionY x = x;;


poleFunction functionY (-1.) 1. 0.001;;

