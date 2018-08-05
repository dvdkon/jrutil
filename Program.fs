// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil.Jdf

[<EntryPoint>]
let main args =
    let batch = parseJdfBatchDir args.[0]
    printfn "Parsing success!\n%A" batch
    0
