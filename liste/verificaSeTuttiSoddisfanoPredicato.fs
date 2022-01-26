let lista = [1;2;-1;4;5;6;7;8;9]
let lista1 = [1;3;5;7;9]


(*
numeroDispari: int -> bool
numeroDispari x = true se x è dispari
*)
let numeroDispari x = 
    if x%2=0 then false
    else true

//oppure in maniera più elegante let numeroDispari x = not (x%2=0)

(*
verificaSeTuttiSoddisfanoPredicato: int list -> (int -> bool) -> bool
verificaSeTuttiSoddisfanoPredicato lst p = true se tutti gli elementi di lst soddisfano p
*)
let rec verificaSeTuttiSoddisfanoPredicato lst p=
        match lst with
        | [] -> true
        | x::rest -> (p x) && (verificaSeTuttiSoddisfanoPredicato rest p)

printfn "%b" (verificaSeTuttiSoddisfanoPredicato lista numeroDispari)
printfn "%b" (verificaSeTuttiSoddisfanoPredicato lista1 numeroDispari)
