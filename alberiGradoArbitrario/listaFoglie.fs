type ’a ntree = Tr of ’a * ’a ntree list

(*
listaFoglie: 'a ntree -> 'a list
listaFoglie tree = lista di foglie di tree
*)
let rec listaFoglie tree = 
    match tree with
    | Tr(x,[]) -> [x]
    | Tr(_,tlist) -> List.concat (List.map listaFoglie tlist)


let nalbero = Tr(1,[Tr(2,[Tr(4,[])]);Tr(3,[])])
printfn "%A" (listaFoglie nalbero)
