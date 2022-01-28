type 'a graph = ('a * 'a) list


let grafo = [(1,2); (1,4); (2,3); (3,5); (2,1); (4,5); (5,1)]


(*
vicini: 'a graph -> 'a -> 'a list
vicini g x = vicini di x nel grafo non orientato g
*)
let rec vicini g x=
    match g with
    |[]->[]
    |(s,t)::rest -> if x=s then t::vicini rest x
                    else if x=t then s::vicini rest x
                            else vicini rest x

printfn "%A" (vicini grafo 3)