type ’a ntree = Tr of ’a * ’a ntree list

(*
listaNodi: 'a ntree -> 'a list
listaNodi tree = lista dei nodi di tree
*)
let rec listaNodi = function
    Tr(x,tlist) ->  x:: (List.concat (List.map listaNodi tlist))


let nalbero = Tr(1,[Tr(2,[Tr(4,[])]);Tr(3,[Tr(5,[Tr(90,[])]);Tr(10,[]);Tr(11,[])])])
printfn "%A" (listaFoglie nalbero)
