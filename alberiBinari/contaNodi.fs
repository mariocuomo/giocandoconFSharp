type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree


(*
numeroNodi: 'a btree -> int
numeroNodi t = ritorna il numero di nodi nell'albero t
*)
let rec numeroNodi t = 
    match t with
    | Empty->0
    | Tr(_,Empty,Empty)-> 1
    | Tr(_,left,right)->1+ numeroNodi left + numeroNodi right



let albero = Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)), Empty))


printfn "%d" (numeroNodi albero)