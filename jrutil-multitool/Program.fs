// This file is part of JrUtil` and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

open System.IO
open Docopt

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil-multitool.exe jdf-to-gtfs <JDF-in-dir> <GTFS-out-dir>
    jrutil-multitool.exe czptt-to-gtfs <CzPtt-in-file> <GTFS-out-dir>
    jrutil-multitool.exe load-gtfs-to-db <db-connstr> <GTFS-in-dir>
    jrutil-multitool.exe --help

Passing - to an input path parameter will make most jrutil commands read
input filenames from stdin. Each result will be output into a sequentially
numbered directory.
"""

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
    withProcessedArgs docstring args (fun args ->
        if argFlagSet args "jdf-to-gtfs" then
            let jdfPar = Jdf.jdfBatchDirParser ()
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (argValue args "<JDF-in-dir>")
                       (argValue args "<GTFS-out-dir>")
            |> Seq.iter (fun (inpath, out) ->
                printfn "Processing %s" inpath
                try
                    let jdf = jdfPar inpath
                    // TODO: Allow choice for stopIdsCis
                    let gtfs = JdfToGtfs.getGtfsFeed false jdf
                    gtfsSer out gtfs
                with
                    | e -> printfn "Error while processing %s:\n%A" inpath e
            )
        else if argFlagSet args "czptt-to-gtfs" then
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (argValue args "<CzPtt-in-file>")
                       (argValue args "<GTFS-out-dir>")
            |> Seq.iter (fun (inpath, out) ->
                printfn "Processing %s" inpath
                try
                    let czptt = CzPtt.parseFile inpath
                    let gtfs = CzPtt.gtfsFeed czptt
                    gtfsSer out gtfs
                with
                    | e -> printfn "Error while processing %s:\n%A" inpath e
            )
        else if argFlagSet args "load-gtfs-to-db" then
            let dbConnStr = argValue args "<db-connstr>"
            let indir = argValue args "<GTFS-in-dir>"

            use dbConn = getPostgresqlConnection dbConnStr
            dbConn.Open()

            try
                Gtfs.sqlCreateGtfsTables dbConn
                Gtfs.sqlLoadGtfsFeed dbConn indir
            with
                | e -> printfn "Error while processing:\n%A" e

            dbConn.Close()
        else printfn "%s" docstring
        0
    )
