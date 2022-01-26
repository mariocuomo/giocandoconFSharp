type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree

let albero = Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)), Empty))


(*
massimo: int -> int -> int
massimo x y = massimo valore tra x e y
*)
let massimo (x:int, y:int) =
    if x>y then x
    else y


(*
altezza: 'a btree -> int
altezza t = ritorna l'altezza dell'albero t
*)
let rec altezza tree =
    match tree with
    |Empty -> 0
    |Tr(_,Empty,Empty)->0
    |Tr(_,left,right)-> 1 + massimo (altezza left, altezza right)

printfn "%d" (altezza albero)