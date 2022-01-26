let lista = [1;2;3;4;5;6;7;8;9]

(*
massimo: int -> int -> int
massimo x y = massimo valore tra x e y
*)
let massimo x y =
    if x>y then x
    else y

(*
massimoLst: int list -> int
massimoLst lst = massimo valore in lst
*)
let rec massimoLst lst =
        match lst with
        | [] -> raise (System.ArgumentException("Lista vuota"))
        | [x] -> x
        | x::[y] -> massimo x y
        | x::rest -> massimo x (massimoLst rest)

printfn "%d" (massimoLst lista)
