open JrUtil.Jdf

[<EntryPoint>]
let main args =
    let batch = parseJdfBatchDir args.[0]
    printfn "Parsing success!\n%A" batch
    0
