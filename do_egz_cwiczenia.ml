let rec cos l =
match l with
[] -> ([], [])
| h::t -> match cos t with
            (l1, l2) -> (h::l1, l2);;

cos [6;5;4;3;2;1];;
