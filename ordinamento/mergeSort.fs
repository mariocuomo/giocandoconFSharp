(*
merge: 'a list * 'a list -> 'a list
merge (lst1,lst2) = lista ordinata che si ottiene fondendo le due liste lst1 lst2
*)
let rec merge (lst1,lst2) =
    match (lst1,lst2) with
    |([],[])->[]
    |([x],[y])->if x<y then [x;y] else [y;x]
    |(lst,[])->lst
    |([],lst)->lst
    |(x::rest1, y::rest2)->if x<y then x:: (merge (rest1,(y::rest2)))
                            else y:: (merge ((x::rest1),rest2))

(*
mergeSort: 'a list -> 'a list
mergeSort lst = ordina in ordine crescente la lista lst utilizzando l'algoritmo Merge Sort
*)
let rec mergeSort lst = 
    match lst with
    |[]->[]
    |[x]->[x]
    |lst -> let (lst1,lst2) = List.splitAt (lst.Length / 2) lst 
            in merge(mergeSort lst1,mergeSort lst2)


let l = [100;90;1;3;2;34;1;0;30;11;9;4;32;4]

printfn "%A" (mergeSort l)