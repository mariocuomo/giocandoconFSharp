(*
minimo: 'a*'a -> 'a'
minimo (x,y) = minimo valore tra x e y
*)
let minimo (x,y) = 
    if x<y then x
    else y


(*
rimuovi: 'a list -> 'a -> 'a list
rimuovi lst x = rimuove la prima occorrenza di x in lst
*)
let rec rimuovi lst x =
    match lst with
    |[]->[]
    |y::rest->if y=x then rest
              else y::rimuovi rest x

(*
minimoLista: 'a list -> 'a
minimoLista lst = ritorna il valore minimo in lst
*)
let rec minimoLista lst = 
    match lst with
    |[]->failwith "lista vuota"
    |[x]->x
    |x::rest -> minimo (x, minimoLista rest)


(*
selectionSort: 'a list -> 'a list
selectionSort lst = ordina in ordine crescente la lista lst utilizzando l'algoritmo Selection Sort
*)
let rec selectionSort lst =
    match lst with
    |[]->[]
    |lst -> let m = minimoLista lst 
            in m :: selectionSort (rimuovi lst m)

let llll = [2;34;1;0;30;11;9;4;32;4]

printfn "%A" (selectionSort llll)