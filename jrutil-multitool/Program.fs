// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open System.IO
open Docopt
open FSharp.Data

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil-multitool.exe jdf-to-gtfs [--stop-coords-by-id=FILE] <JDF-in-dir> <GTFS-out-dir>
    jrutil-multitool.exe czptt-to-gtfs <CzPtt-in-file> <GTFS-out-dir>
    jrutil-multitool.exe --help

Options:
    --stop-coords-by-id=FILE CSV file assigning coordinates to stops by ID

Passing - to an input path parameter will make most jrutil commands read
input filenames from stdin. Each result will be output into a sequentially
numbered directory.
"""

type StopCoordsById = CsvProvider<
    HasHeaders = false,
    Schema = "id(string), lat(decimal), lon(decimal)">

let stdinLinesSeq () =
    Seq.initInfinite (fun i -> stdin.ReadLine())
    |> Seq.takeWhile (fun l -> l <> null)

let inOutFiles inpath outpath =
    if inpath = "-" then
        stdinLinesSeq ()
        |> Seq.mapi (fun i l -> (l, Path.Combine(outpath, string i)))
    else
        seq [(inpath, outpath)]

let gtfsWithCoords stopCoordsByIdPath (gtfs: GtfsModel.GtfsFeed) =
    match stopCoordsByIdPath with
    | Some p ->
        let coords =
            StopCoordsById.Load(Path.GetFullPath(p)).Rows
            |> Seq.map (fun r -> r.Id, (r.Lat, r.Lon))
            |> Map
        { gtfs with
            stops =
                gtfs.stops
                |> Array.map (fun s ->
                    match coords |> Map.tryFind s.id with
                    | Some (lat, lon) -> { s with
                                             lat = Some lat
                                             lon = Some lon }
                    | None -> s)
        }
    | _ -> gtfs

[<EntryPoint>]
let main (args: string array) =
    withProcessedArgs docstring args (fun args ->
        let stopCoordsByIdPath = optArgValue args "--stop-coords-by-id"
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
                    let gtfs =
                        JdfToGtfs.getGtfsFeed false jdf
                        |> gtfsWithCoords stopCoordsByIdPath

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
                    let gtfs =
                        czptt.CzpttcisMessage
                        |> Option.get
                        |> CzPtt.gtfsFeed
                        |> gtfsWithCoords stopCoordsByIdPath
                    gtfsSer out gtfs
                with
                    | e -> printfn "Error while processing %s:\n%A" inpath e
            )
        else printfn "%s" docstring
        0
    )
