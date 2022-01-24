let lista = [1;2;3;4;5;6;7;8;9]
let lista1 = [1;2;3;-4;5;6;7;8;9]


(*
verificaSeTuttiPositivi: int list -> bool
verificaSeTuttiPositivi lst = true se tutti i valori in lst sono maggiori di 0 (escluso)
*)
let rec verificaSeTuttiPositivi lst =
        match lst with
        | [] -> true
        | x::rest -> (x>0) && (verificaSeTuttiPositivi rest)


printfn "%b" (verificaSeTuttiPositivi lista)
printfn "%b" (verificaSeTuttiPositivi lista1)