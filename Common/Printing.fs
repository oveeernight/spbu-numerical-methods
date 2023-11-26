namespace Common

module Printing =
    let printFunctionTable name points values =
        printfn "%s:" name
        printf "%4s" "x"
        points |> Seq.iter (printf "|%15.9f")
        printfn ""
        printf "%4s" "f(x)"
        values |> Seq.iter (printf "|%15.9f")
        printfn ""

