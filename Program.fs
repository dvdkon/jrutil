open JrUtil.JdfModel
open JrUtil.JdfCsvParser

[<EntryPoint>]
let main args =
    printfn "Trying to parse file %s as Routes" args.[0]
    let res = parseCsvFile<Route> args.[0]
    printfn "Parsing success! %A" res
    0
