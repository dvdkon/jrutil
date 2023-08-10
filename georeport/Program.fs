// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
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
    georeport.exe jdf-to-stop-names <JDF-in-dir>

report options:
    --connstr=CONNSTR         Npgsql connection string
    --rail-stops=PATH         Path to CSV of railway stops
    --other-stops=PATH        Path to CSV of non-railway stops
    --rail-ext-sources=PATH   Folder with external geodata sources for railway stops
    --other-ext-sources=PATH  Folder with external geodata sources for non-railway stops
    --overpass-url=URL        URL of OSM Overpass API instance (optional)
    --cache-dir=DIR           Overpass result cache directory [default: /tmp]
    --logfile=PATH            Output path for logfile (default is console output)

External sources should be in CSV format, with columns being lat,lon,stop name.
Railway stops CSV has columns sr70,name
Other stops CSV has just one column, name
Railway external source is a CSV with columns sr70,name,lat,lon
Non-railway external source is a CSV with columns name,lat,lon

Setting an empty cache dir will disable caching
"""

let defaultOverpassUrl = "https://lz4.overpass-api.de/api/interpreter"

[<EntryPoint>]
let main argv =
    withProcessedArgs docstring argv (fun args ->
        setupLogging (optArgValue args "--logfile") ()
        if argFlagSet args "report" then
            let overpassUrl = optArgValue args "--overpass-url"
                              |> Option.defaultValue defaultOverpassUrl


            let railStopsPath = argValue args "--rail-stops"
            let otherStopsPath = argValue args "--other-stops"
            let railExtSourcesDir = argValue args "--rail-ext-sources"
            let otherExtSourcesDir = argValue args "--other-ext-sources"
            let cacheDir = argValue args "--cache-dir"

            let dbConnStrMod = (argValue args "--connstr") + ";CommandTimeout=0"
            Log.Information("Preparing database")
            let conn = getPostgresqlConnection dbConnStrMod
            cleanAndSetSchema conn "georeport"
            initSqlTables conn

            Log.Information("Loading stop lists")
            loadStopListsToSql conn railStopsPath otherStopsPath
            Log.Information("Loading OSM data")
            loadOsmDataToSql conn overpassUrl cacheDir
            Log.Information("Loading external data")
            loadExternalDataToSql conn railExtSourcesDir otherExtSourcesDir

            Log.Information("Creating report")
            let railStopMatches = getRailStopsMatches conn
            let otherStopMatches = getOtherStopsMatches conn

            let page = resultPage railStopMatches otherStopMatches
            let html = renderHtmlDocument page
            printfn "%s" html

            0
        else if argFlagSet args "jdf-to-stop-names" then
            argValue args "<JDF-in-dir>"
            |> jdfStopNames
            |> Seq.iter (fun s -> printfn "%s" s)

            0
        else
            printfn "No command specified!"
            1
    )
