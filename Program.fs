// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil

[<EntryPoint>]
let main args =
    let batch = Jdf.parseJdfBatchDir args.[0]
    printfn "Parsing success!\n%A" batch
    //let doc = CzPtt.parseFile args.[0]
    //printfn "Parsing success!\n%A" doc
    0
