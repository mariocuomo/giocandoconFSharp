let ll = [1;2;3;4;5;6;]
let l2l = [1;-2;3;4;-5;6;]


(*
listaAlQuadrato: int list -> int list
listaAlQuadrato lst = lista che si ottiene elevando al quadrato ogni valore di lst
*)
let rec listaAlQuadrato lst = 
    match lst with
    |[]->[]
    |x::rest->x*x::listaAlQuadrato rest

let _listaAlQuadrato lst = List.map (fun x -> x*x) lst



(*
listaCoppieAlQuadrato: int list -> (int*int) list
listaCoppieAlQuadrato lst = lista di coppie (x,y) in cui x è un valore di lst e y e il quadrato di x
*)
let listaCoppieAlQuadrato lst = List.map (fun x -> (x,x*x)) lst


(*
trasformaInPositivo: int list -> int list
trasformaInPositivo lst = lista che si ottiene applicando il modulo a ogni elemento di lst
*)
let trasformaInPositivo lst = List.map(fun x -> if x<0 then (-1)*x else x) lst


(*
primiElementiDaListaDiCoppie: (int*int) list -> int list
primiElementiDaListaDiCoppie lst = lista che si ottiene filtrando solo il primo elemento di ogni elemento di lst 
*)
let primiElementiDaListaDiCoppie lst = List.map(fun (x,y) -> x) lst




let ll = [1;2;3;4;5;6;]
printfn "%A" (_listaAlQuadrato ll)

let l2l = [1;-2;3;4;-5;6;]
printfn "%A" (trasformaInPositivo l2l)

let l3l = [1,2;-2,3;3,4;4,2;-5,1;6,3;]
printfn "%A" (primiElementiDaListaDiCoppie l3l)

