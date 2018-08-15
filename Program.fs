// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil
open System
open System.IO

[<EntryPoint>]
let main args =
    //let batch = Jdf.parseJdfBatchDir args.[0]
    //printfn "Parsing success!%A" batch
    //let doc = CzPtt.parseFile args.[0]
    //printfn "Parsing success!%A" doc

    (*let feedInfo: GtfsModel.FeedInfo = {
        publisherName = "JrUtil"
        publisherUrl = "https://gitlab.com/dvdkon/jrutil"
        lang = "cs"
        startDate = Some (new DateTime())
        endDate = None
        version = Some "0.1"
    }
    printfn "Header: %s" GtfsCsvSerializer.getHeader<GtfsModel.FeedInfo>
    printfn "CSV:\n%s" (GtfsCsvSerializer.serializeRows [|feedInfo|]
                          |> String.concat "\n")*)

    (*printfn "Parsing all JDF batches in %s\n" args.[0]
    Directory.GetDirectories(args.[0])
    |> Array.iter (fun d ->
        let batchNum = d.Split("/") |> Array.tail
        let versionFile = File.ReadAllText(Path.Combine(d, "VerzeJDF.txt"))
        // The final version will of course be better ;)
        if not (versionFile.Contains("1.11"))
        then () //printfn "Skipping %s due to older version..." d
        else // printfn "Parsing %s..." d
             let batch = Jdf.parseJdfBatchDir d
             if batch.routes |> Array.exists (fun r -> r.usesStopPosts)
             then printfn "%s uses stop posts!" d
             else ()
    )*)

    let batch = Jdf.parseJdfBatchDir args.[0]
    let feed = JdfToGtfs.getGtfsFeed batch
    Gtfs.gtfsFeedToFolder args.[1] feed

    0
