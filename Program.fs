// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil
open System
open JrUtil

[<EntryPoint>]
let main args =
    //let batch = Jdf.parseJdfBatchDir args.[0]
    //printfn "Parsing success!\n%A" batch
    //let doc = CzPtt.parseFile args.[0]
    //printfn "Parsing success!\n%A" doc

    let feedInfo: GtfsModel.FeedInfo = {
        publisherName = "JrUtil"
        publisherUrl = "https://gitlab.com/dvdkon/jrutil"
        lang = "cs"
        startDate = Some (new DateTime())
        endDate = None
        version = Some "0.1"
    }
    printfn "Header: %s" GtfsCsvSerializer.getHeader<GtfsModel.FeedInfo>
    printfn "CSV:\n%s\n" (GtfsCsvSerializer.serializeRows [|feedInfo|]
                          |> String.concat "\n")
    0
