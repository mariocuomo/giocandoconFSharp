type ’a ntree = Tr of ’a * ’a ntree list


(*
massimo: int*int -> int
massimo (x,y) = massimo valore tra x e y
*)
let massimo (x:int, y:int) =
    if x>y then x
    else y

(*
altezzaAlbero: 'a ntree -> int
altezzaAlbero tree = altezza dell'albero t

altezzaAlberoList: 'a ntree list-> 'a -> 'a list
altezzaAlberoList tlist = altezza dell'albero più profondo in tlist
*)
let rec altezzaAlbero tree = 
    match tree with
    |Tr(_,[])->0
    |Tr(_,tlist)->1 + altezzaAlberoList tlist
and altezzaAlberoList tlist =
    match tlist with
    |[]->0
    |x::rest-> massimo (altezzaAlbero x, altezzaAlberoList rest)


let nalbero = Tr(1,[Tr(2,[Tr(5,[])]);Tr(3,[Tr(4,[])]);Tr(10,[Tr(1,[Tr(111,[])])])])
printfn "%d" (altezzaAlbero nalbero)