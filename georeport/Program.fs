// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open Giraffe.GiraffeViewEngine
open Serilog

open JrUtil.Utils
open JrUtil.SqlRecordStore
open GeoReport.Processing
open GeoReport.HtmlPage
open GeoReport.StopListGen

let docstring = (fun (s: string) -> s.Trim()) """
georeport, a tool for asessing coverage of stop geodata

Usage:
    georeport.exe report [options]
    georeport.exe jdf-to-stop-names [--cz-sk-only] <JDF-in-dir>
    georeport.exe czptt-to-stop-list <CZPTT-in-dir>

report options:
    --rail-stops=PATH         Path to CSV of railway stops
    --other-stops=PATH        Path to CSV of non-railway stops
    --rail-ext-sources=PATH   Folder with external geodata sources for railway stops
    --other-ext-sources=PATH  Folder with external geodata sources for non-railway stops
    --cz-pbf=URL              OSM data for Czech Republic
    --logfile=PATH            Output path for logfile (default is console output)
    --cz-sk-only              Don't include "foreign" stops in output

External sources should be in CSV format, with columns being lat,lon,stop name.
Railway stops CSV has columns sr70,name
Other stops CSV has just one column, name
Railway external source is a CSV with columns sr70,name,lat,lon
Non-railway external source is a CSV with columns name,lat,lon

Setting an empty cache dir will disable caching
"""

[<EntryPoint>]
let main argv =
    withProcessedArgs docstring argv (fun args ->
        setupLogging (optArgValue args "--logfile") ()
        if argFlagSet args "report" then
            let czPbfPath = argValue args "--cz-pbf"
            let railStopsPath = argValue args "--rail-stops"
            let otherStopsPath = argValue args "--other-stops"
            let railExtSourcesDir = argValue args "--rail-ext-sources"
            let otherExtSourcesDir = argValue args "--other-ext-sources"

            Log.Information("Getting rail stop matches")
            let railStopMatches =
                getRailStopsMatches czPbfPath
                                    railStopsPath
                                    railExtSourcesDir
            Log.Information("Getting other stop matches")
            let otherStopMatches =
                getOtherStopsMatches czPbfPath
                                     otherStopsPath
                                     otherExtSourcesDir

            Log.Information("Creating report")
            let page = resultPage railStopMatches otherStopMatches
            let html = renderHtmlDocument page
            printfn "%s" html

            0
        else if argFlagSet args "jdf-to-stop-names" then
            argValue args "<JDF-in-dir>"
            |> jdfStopNames (argFlagSet args "--cz-sk-only")
            |> Seq.iter (fun s -> printfn "%s" s)

            0
        else if argFlagSet args "czptt-to-stop-list" then
            argValue args "<CZPTT-in-dir>"
            |> czpttStopList
            |> Seq.iter (fun (i, n) -> printfn "%s,\"%s\"" i n)

            0
        else
            printfn "No command specified!"
            1
    )
