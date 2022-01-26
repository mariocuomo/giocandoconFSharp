let albero3 = Tr(1,Tr(2,Tr(5,Tr(8,Tr(20,Empty,Tr(29,Empty,Tr(15,Empty,Empty))),Empty),Empty),Tr(11,Empty,Empty)),Tr(3,Tr(6,Empty,Empty),Tr(4,Empty,Tr(10,Empty,Tr(19,Empty,Empty)))))



(*
trovaCamminoRadiceFoglia: 'a btree -> 'a list
trovaCamminoRadiceFoglia t f: lista che rappresenta il cammino dalla radice a una foglia f. Se non esiste è sollevato un errore
*)
let rec trovaCamminoRadiceFoglia tree target =
    match tree with
    |Empty -> failwith "Errore"
    |Tr(x,Empty,Empty)-> if x=target then [x] else failwith "Errore"
    |Tr(x,left,right)-> x::(try trovaCamminoRadiceFoglia left target
                            with Failure "Errore"-> trovaCamminoRadiceFoglia right target)

printfn "%A" (trovaCamminoRadiceFoglia albero3 19)
