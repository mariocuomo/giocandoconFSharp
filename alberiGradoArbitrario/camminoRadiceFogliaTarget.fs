type ’a ntree = Tr of ’a * ’a ntree list


(*
ricercaCamminoRadiceFoglia: 'a ntree -> 'a -> 'a list
ricercaCamminoRadiceFoglia tree target = cammino dalla radice a una foglia target in tree

ricercaCamminoRadiceFogliaList: 'a ntree list-> 'a -> 'a list
ricercaCamminoRadiceFogliaList tlist target = cammino che parte da uno delle radici degli alberi di tlist e arriva a una foglia target
*)
let rec ricercaCamminoRadiceFoglia tree target =
    match tree with 
    |Tr(x,[])-> if x=target then [x] 
                else failwith "Errore"
    |Tr(x,tlist)->x::ricercaCamminoRadiceFogliaList tlist target
and ricercaCamminoRadiceFogliaList lst target =
    match lst with
    |[]->failwith "Errore"
    |t::rest -> try ricercaCamminoRadiceFoglia t target
                with Failure "Errore"->ricercaCamminoRadiceFogliaList rest target


let nalbero = Tr(1,[Tr(2,[Tr(4,[])]);Tr(3,[])])
printfn "%A" (ricercaCamminoRadiceFoglia nalbero 4)