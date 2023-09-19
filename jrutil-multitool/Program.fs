// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open System.IO
open FSharp.Data
open Serilog
open Serilog.Context

open JrUtil
open JrUtil.GeoData
open JrUtil.Utils

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil-multitool.exe jdf-to-gtfs [options] <JDF-in-dir> <GTFS-out-dir>
    jrutil-multitool.exe czptt-to-gtfs [options] <CzPtt-in-file> <GTFS-out-dir>
    jrutil-multitool.exe fix-jdf [options] <JDF-in-dir> <JDF-out-dir>
    jrutil-multitool.exe merge-jdf [options] <JDF-out-dir> <JDF-in-dir>...
    jrutil-multitool.exe --help

Options:
    --stop-coords-by-id=FILE    CSV file assigning coordinates to stops by ID
    -g --ext-geodata=FILE       CSV file with stop positions (name,lat,lon,region)
    -o --cz-pbf=URL             OSM data for Czech Republic
    -l --logfile=FILE           Logfile

Passing - to an input path parameter will make most jrutil commands read
input filenames from stdin. Each result will be output into a sequentially
numbered directory.
"""

type StopCoordsById = CsvProvider<
    HasHeaders = false,
    Schema = "id(string), lat(decimal), lon(decimal)">

let stdinLinesSeq () =
    Seq.initInfinite (fun _ -> stdin.ReadLine())
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
        setupLogging (optArgValue args "--logfile") ()

        let stopCoordsByIdPath = optArgValue args "--stop-coords-by-id"
        if argFlagSet args "jdf-to-gtfs" then
            let jdfPar = Jdf.jdfBatchDirParser ()
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (argValues args "<JDF-in-dir>" |> Seq.head)
                       (argValue args "<GTFS-out-dir>")
            |> Seq.iter (fun (inpath, out) ->
                Log.Information("Processing {Batch}", inpath)
                try
                    Log.Information("Reading JDF")
                    let jdf = jdfPar (Jdf.FsPath inpath)
                    // TODO: Allow choice for stopIdsCis
                    Log.Information("Converting to GTFS")
                    let gtfs =
                        JdfToGtfs.getGtfsFeed false jdf
                        |> gtfsWithCoords stopCoordsByIdPath

                    Log.Information("Writing GTFS")
                    gtfs
                    |> Gtfs.fillStandardRequiredFields
                    |> gtfsSer out
                    Log.Information("Finished!")
                with
                    | e -> Log.Error(e, "Error while processing {Batch}", inpath)
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
        else if argFlagSet args "fix-jdf" then
            let inDir = argValues args "<JDF-in-dir>" |> Seq.head
            let outDir = argValue args "<JDF-out-dir>"
            let geodataPath = optArgValue args "--ext-geodata"
            let czPbf = optArgValue args "--cz-pbf"

            let extStopsToMatch =
                geodataPath
                |> Option.map (fun gdp ->
                    Utils.logWrappedOp "Reading external stops" <| fun () ->
                        ExternalCsv.OtherStops.Parse(File.ReadAllText(gdp))
                        |> ExternalCsv.otherStopsForJdfMatch)
                |> Option.defaultValue [||]
            let osmStopsToMatch =
                czPbf
                |> Option.map (fun pbf ->
                    Utils.logWrappedOp "Reading OSM stops" <| fun () ->
                        Osm.getCzOtherStops pbf
                        |> Osm.czOtherStopsForJdfMatch)
                |> Option.defaultValue [||]
            let stopMatcher = new StopMatcher.StopMatcher<_>(Array.concat [
                extStopsToMatch
                osmStopsToMatch
            ])

            let jdfPar = Jdf.jdfBatchDirParser ()
            let jdfWri = Jdf.jdfBatchDirWriter ()
            Jdf.findJdfBatches inDir
            |> Seq.iter (fun (batchPath, batchDir) ->
                let batchName = Path.GetFileNameWithoutExtension(batchPath)
                use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
                Log.Information("Processing JDF batch {BatchPath}", batchPath)

                let batch = jdfPar batchDir
                let batchFixed, stopMatches =
                    JdfFixups.fixPublicCisJrBatch stopMatcher batch

                let stopsWithMatches = Array.zip batchFixed.stops stopMatches
                let batchWithLocations =
                    JdfFixups.addStopLocations batchFixed stopsWithMatches
                Seq.concat [
                    batchFixed.tripStops
                    |> Seq.groupBy (fun ts -> ts.routeId, ts.tripId)
                    |> Seq.map snd
                    // Take one trip most likely to contain all stops' km
                    // distances (testing all takes too much time)
                    |> Seq.sortByDescending Seq.length
                    |> Seq.head
                    |> fun ts ->
                        JdfFixups.checkMatchDistances
                            (Seq.toArray ts) stopsWithMatches

                    JdfFixups.checkMissingRegionsCountries batchFixed
                ]
                |> Seq.iter (fun msg -> Log.Write(msg))

                let fixedOutDir = Path.Combine(outDir, batchName)
                Directory.CreateDirectory(fixedOutDir) |> ignore
                jdfWri (Jdf.FsPath fixedOutDir) batchWithLocations)
            Log.Information("Finished!")
        else if argFlagSet args "merge-jdf" then
            let outDir = argValue args "<JDF-out-dir>"

            let merger = JdfMerger.JdfMerger()
            let jdfPar = Jdf.jdfBatchDirParser ()
            let jdfWri = Jdf.jdfBatchDirWriter ()

            for inDir in argValues args "<JDF-in-dir>" do
                for batchPath, batchDir in Jdf.findJdfBatches inDir do
                    let batchName = Path.GetFileNameWithoutExtension(batchPath)
                    use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
                    Log.Information("Merging JDF batch {BatchPath}", batchPath)

                    let batch = jdfPar batchDir
                    merger.add(batch)

            Log.Information("Resolving route overlaps")
            merger.resolveRouteOverlaps()

            Log.Information("Writing merged JDF")
            jdfWri (Jdf.FsPath outDir) merger.batch
            Log.Information("Finished!")
        else printfn "%s" docstring
        0
    )
