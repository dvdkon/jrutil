// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open JrUtil
open System
open System.IO
open DocoptNet

let docstring = """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil.exe jdf_to_gtfs <JDF_in_dir> <GTFS_out_dir>
    jrutil.exe czptt_to_gtfs <CzPtt_in_file> <GTFS_out_dir>
"""

[<EntryPoint>]
let main args =
    try
        let args = Docopt().Apply(docstring, args)
        if args.["jdf_to_gtfs"].IsTrue then
            let jdf = Jdf.parseJdfBatchDir (unbox args.["<JDF_in_dir>"].Value)
            let gtfs = JdfToGtfs.getGtfsFeed jdf
            Gtfs.gtfsFeedToFolder (unbox args.["<GTFS_out_dir>"].Value) gtfs
        if args.["czptt_to_gtfs"].IsTrue then
            let czptt = CzPtt.parseFile (unbox args.["<CzPtt_in_file>"].Value)
            let gtfs = CzPtt.gtfsFeed czptt
            Gtfs.gtfsFeedToFolder (unbox args.["<GTFS_out_dir>"].Value) gtfs
        0
    with
    | :? DocoptBaseException as e ->
        printfn "%s" e.Message
        1
