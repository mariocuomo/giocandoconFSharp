type 'a btree = Empty | Tr of 'a * 'a btree * 'a btree


let albero = Tr(1,Tr(2,Tr(5,Tr(8,Tr(20,Empty,Tr(29,Empty,Tr(15,Empty,Empty))),Empty),Empty),Tr(11,Empty,Empty)),Tr(3,Tr(6,Empty,Empty),Tr(4,Empty,Tr(10,Empty,Tr(19,Empty,Empty)))))


(*
verificaAlberoCompleto: 'a btree -> bool
verificaAlberoCompleto t = true se l'albero t è un albero completo

verificaAlberoCompletoRec: 'a btree -> 'a btree -> bool
verificaAlberoCompletoRec t1 t2 = true se t1 e t2 hanno la stessa forma e sono completi entrambi
*)
let verificaAlberoCompleto tree = 
    let rec verificaAlberoCompletoRec t1 t2 =
        match (t1,t2) with
        |(Empty,Empty)->true
        |(Tr(_,_,_),Empty)->false
        |(Empty,Tr(_,_,_))->false
        |(Tr(_,l1,r1),Tr(_,l2,r2))-> (verificaAlberoCompletoRec l1 l2) && (verificaAlberoCompletoRec r1 r2)
    in match tree with
        | Empty->true
        | Tr(_,l,r)->verificaAlberoCompletoRec l r

printfn "%b" (verificaAlberoCompleto albero)
