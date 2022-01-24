let lista = [1;2;3;4;5;6;7;8;9]

let massimo x y =
    if x>y then x
    else y

let rec massimoLst lst =
        match lst with
        | [] -> -14
        | x::[y] -> massimo x y
        | x::rest -> massimo x (massimoLst rest)
