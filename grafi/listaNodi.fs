type 'a graph = ('a * 'a) list

let grafo = [(1,2); (1,4); (2,3); (3,5); (2,1); (4,5); (5,1)]


(*
flatList: 'a * 'a list -> 'a list
flatList lst = lista che si ottiene splittando le coppie (x,y) di lst
*)
let rec flatList lst =
    match lst with
    |[]->[]
    |(x,y)::rest -> x::y::(flatList rest)

(*
getVertici: 'a graph -> 'a list
getVertici g = lista dei nodi del grafo g
*)
let getVertici g = List.distinct (flatList g)


printfn "%A" (getVertici grafo)