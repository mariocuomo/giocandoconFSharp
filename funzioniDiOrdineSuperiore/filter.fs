(*
numeroPari: int -> bool
numeroPari x = true se x è pari
*)
let numeroPari x = x%2=0


(*
filtraLista: int list -> (int->bool) -> int list
filtraLista lst p = lst filtrata con i soli elementi che soddisfano il predicato p
*)
let rec filtraLista lst p = 
    match lst with
    |[]->[]
    |x::rest-> if p (x) then x:: (filtraLista rest p)
                else (filtraLista rest p)

let _filtraLista lst p = List.filter(fun x-> p x)) lst



(*
filtraCoppieDiOpposti: (int*int) list -> (int*int) list
filtraCoppieDiOpposti lst = lst filtrata con le sole coppie (x,y) tali che x+y=0
*)
let filtraCoppieDiOpposti lst = List.filter(fun (x,y)-> x+y=0) lst


let ll = [1;2;3;4;5;6;]
printfn "%A" (filtraLista ll numeroPari)

let l2l = [1,1;2,-2;3,-3;4,1;5,2;6,-2;]
printfn "%A" (filtraCoppieDiOpposti l2l)