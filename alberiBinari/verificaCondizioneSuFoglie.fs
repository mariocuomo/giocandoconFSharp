type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree

let albero = Tr(1, Tr(2, Tr(4,Empty,Empty), Empty), Tr(3, Tr(5, Tr(6,Empty,Empty), Tr(8,Empty,Empty)), Empty))

(*
numeroPari: int -> bool
numeroPari x = true se x è pari
*)
let numeroPari x = x%2=0



(*
verificaCondizioneSuTutteLeFoglie: 'a btree -> 'a -> bool -> bool
verificaCondizioneSuTutteLeFoglie t p = true se tutte le foglie di t soddisfano p
*)
let rec verificaCondizioneSuTutteLeFoglie tree p =
    match tree with
    |Empty ->true
    |Tr(x,Empty,Empty)-> p x
    |Tr(_,left,right)-> verificaCondizioneSuTutteLeFoglie left p && verificaCondizioneSuTutteLeFoglie right p 


printfn "%d" (altezza albero)