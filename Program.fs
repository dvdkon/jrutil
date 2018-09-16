// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil
open System.IO
open DocoptNet

let docstring = """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil.exe jdf_to_gtfs <JDF_in_dir> <GTFS_out_dir>
    jrutil.exe czptt_to_gtfs <CzPtt_in_file> <GTFS_out_dir>
    jrutil.exe merge_gtfs <GTFS_out_dir> <GTFS_in_dir>...

Passing - to an input path parameter will make jrutil read input filenames
from stdin. Each result will be output into a sequentially numbered
directory.
"""

// merge_gtfs' argument order being opposite of other commands' is not
// intentional, it's necessary to work around a "bug" in docopt.net

let stdinLinesSeq () =
    Seq.initInfinite (fun i -> stdin.ReadLine())
    |> Seq.takeWhile (fun l -> l <> null)

let inOutFiles inpath outpath =
    if inpath = "-" then
        stdinLinesSeq ()
        |> Seq.mapi (fun i l -> (l, Path.Combine(outpath, string i)))
    else
        seq [(inpath, outpath)]

[<EntryPoint>]
let main (args: string array) =
    try
        let args = Docopt().Apply(docstring, args)
        if args.["jdf_to_gtfs"].IsTrue then
            let jdfPar = Jdf.jdfBatchDirParser ()
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (unbox args.["<JDF_in_dir>"].Value)
                       (unbox args.["<GTFS_out_dir>"].Value)
            |> Seq.iter (fun (inPath, out) ->
                printfn "%s" inPath
                let jdf = jdfPar inPath
                let gtfs = JdfToGtfs.getGtfsFeed jdf
                gtfsSer out gtfs
            )
        if args.["czptt_to_gtfs"].IsTrue then
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (unbox args.["<CzPtt_in_file>"].Value)
                       (unbox args.["<GTFS_out_dir>"].Value)
            |> Seq.iter (fun (inPath, out) ->
                printfn "Processing %s" inPath
                let czptt = CzPtt.parseFile inPath
                let gtfs = CzPtt.gtfsFeed czptt
                gtfsSer out gtfs
            )
        if args.["merge_gtfs"].IsTrue then
            let infiles =
                args.["<GTFS_in_dir>"].AsList
                |> Seq.cast<ValueObject>
                |> Seq.map (fun vo -> unbox vo.Value)
            let infiles =
                if infiles |> Seq.tryHead = Some "-"
                then stdinLinesSeq()
                else infiles

            let feedParser = Gtfs.gtfsParseFolder ()
            let mergedFeed = new GtfsMerge.MergedFeed()
            infiles
            |> Seq.iter (fun inpath ->
                printfn "Processing %s" inpath
                let feed = feedParser inpath
                mergedFeed.InsertFeed feed
            )
            let feedSerializer = Gtfs.gtfsFeedToFolder ()
            let outPath = (unbox args.["<GTFS_out_dir>"].Value)
            feedSerializer outPath (mergedFeed.ToGtfsFeed())
        0
    with
    | :? DocoptBaseException as e ->
        printfn "%s" e.Message
        1
