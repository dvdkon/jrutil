// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil
open System
open System.IO
open DocoptNet
open JrUtil

let docstring = """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil.exe jdf_to_gtfs <JDF_in_dir> <GTFS_out_dir>
    jrutil.exe czptt_to_gtfs <CzPtt_in_file> <GTFS_out_dir>

Passing - to an input path parameter will make jrutil read input filenames
from stdin. Each result will be output into a sequentially numbered
directory.
"""

let inOutFiles inpath outpath =
    if inpath = "-" then
        Seq.initInfinite (fun i ->
            (stdin.ReadLine(), Path.Combine(outpath, string i)))
        |> Seq.takeWhile (fun (i, o) -> i <> null)
    else
        seq [(inpath, outpath)]

[<EntryPoint>]
let main args =
    printfn "JrUtil started!"
    try
        let args = Docopt().Apply(docstring, args)
        if args.["jdf_to_gtfs"].IsTrue then
            let jdfPar = Jdf.jdfBatchDirParser ()
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (unbox args.["<JDF_in_dir>"].Value)
                       (unbox args.["<GTFS_out_dir>"].Value)
            |> Seq.iter (fun (inPath, out) ->
                let jdf = jdfPar inPath
                let gtfs = JdfToGtfs.getGtfsFeed jdf
                gtfsSer out gtfs
            )
        if args.["czptt_to_gtfs"].IsTrue then
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (unbox args.["<CzPtt_in_file>"].Value)
                       (unbox args.["<GTFS_out_dir>"].Value)
            |> Seq.iter (fun (inPath, out) ->
                let czptt = CzPtt.parseFile inPath
                let gtfs = CzPtt.gtfsFeed czptt
                gtfsSer out gtfs
            )
        0
    with
    | :? DocoptBaseException as e ->
        printfn "%s" e.Message
        1
