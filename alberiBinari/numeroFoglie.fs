type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree

let albero = Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(7,Empty,Empty)), Empty))



(*
numeroFoglie: 'a btree -> int
numeroFoglie t = ritorna il numero di foglie in t
*)
let rec numeroFoglie tree =
    match tree with
    |Empty->0
    |Tr(_,Empty,Empyt)->1
    |Tr(_,left,right)->numeroFoglie left + numeroFoglie right

printfn "%d" (numeroFoglie albero)