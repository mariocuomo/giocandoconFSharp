(*
conta: 'a -> 'a list -> int
conta x lst = numero di occorrenze di x nella lista lst
*)
let rec conta x lst=
    match lst with
    |[]->0
    |y::rest->if x=y then 1 + conta x rest
                else conta x rest

(*
contaOccorrenze: 'a list -> ('a * int) list
contaOccorrenze lst = lista di coppie (a,b) dove a è un valore distinto di lst e b è il numero di occorrenze di a
*)
let contaOccorrenze lst = List.distinct (List.map (function x-> (x,conta x lst)) lst)


let l = [1;1;1;3;4;4;0]

printfn "%A" (contaOccorrenze l)
