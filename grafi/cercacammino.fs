type 'a graph = ('a * 'a) list


(*
contiene: 'a -> 'a list -> bool
contiene x lst = true se x è contenuto in lst
*)
let rec contiene x lst =
    match lst with
    |[]->false
    |k::rest->x=k || (contiene x rest)

(*
cercaCammino: 'a graph -> 'a -> 'a -> 'a list
cercaCammino g start goal =  cammino nel grafo g da start a goal

from_node: 'a -> 'a list
from_node start visited =  cammino nel grafo g da start a goal che non passa per nodi in visited

from_list: 'a list -> 'a list
from_list visited lst =  cammino nel grafo g che parte da uno dei nodi di lst fino a goal e che non passa per nodi in visited
*)
let cercaCammino g start goal = 
    let rec from_node start visited=
        if contiene start visited then failwith "errore"
        else if goal=start then [start]
             else start:: from_list (start::visited) (vicini g start)
    and from_list visited lst= 
        match lst with
        |[]->failwith "errore"
        |x::rest ->try from_node x visited
                   with Failure "errore" -> from_list visited rest
    in from_node start []


let grafo = [(1,2); (2,3); (3,4); (1,4)]

printfn "%A" (cercaCammino grafo 2 1)
