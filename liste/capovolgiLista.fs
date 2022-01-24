let lista = [1;2;3;4;5;6;7;8;9]

(*
capovolgi: a list -> a list
capovolgi lst = ritorna la list lst capovolta
*)
let rec capovolgi lst = 
    match lst with
    | [] -> []
    | [x] -> [x]
    | x::rest -> (capovolgi lst) @ [x]