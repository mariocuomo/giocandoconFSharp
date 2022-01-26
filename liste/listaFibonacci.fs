let fibonacciLista = [1;1;2;3;5;8;13;21;34;55;89;144;233;377;610]


(*
listaComeSerieDiFibonacci: int list -> bool
listaComeSerieDiFibonacci lst = true se la lista è parte della serie di Fibonacci
*)
let rec listaComeSerieDiFibonacci lst =
    match lst with 
    |[]->true
    |[x]-> x=1
    |x::y::[]-> x=1 && y=1
    |x::y::z::[] -> x+y=z
    |x::y::z::rest -> x+y=z && (listaComeSerieDiFibonacci (y::z::rest))

printfn "%b" (listaComeSerieDiFibonacci fibonacciLista)
