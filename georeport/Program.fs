// This file is part of JrUtil and is licenced under the GNU GPLv2 or later
// (c) 2019 David Koňařík

open System
open System.IO

open Giraffe.GiraffeViewEngine

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open GeoReport.Processing
open GeoReport.HtmlPage

let docstring = (fun (s: string) -> s.Trim()) """
georeport, a tool for asessing coverage of stop geodata

Usage:
    georeport.exe [options]

Options:
    --connstr=CONNSTR         Npgsql connection string
    --rail-stops=PATH         Path to CSV of railway stops
    --other-stops=PATH        Path to CSV of non-railway stops
    --rail-ext-sources=PATH   Folder with external geodata sources for railway stops
    --other-ext-sources=PATH  Folder with external geodata sources for non-railway stops
    --overpass-url=URL        URL of OSM Overpass API instance (optional)
    --cache-dir               Overpass result cache directory (default /tmp)

External sources should be in CSV format, with columns being lat,lon,stop name.
Railway stops CSV has columns sr70,name
Other stops CSV has just one column, name (must be in quotes)
Railway external source is a CSV with columns sr70,name,lat,lon
Non-railway external source is a CSV with columns name,lat,lon

Setting an empty cache dir will disable caching
"""

let defaultOverpassUrl = "https://lz4.overpass-api.de/api/interpreter"

[<EntryPoint>]
let main argv =
    withProcessedArgs docstring argv (fun args ->
        let overpassUrl = optArgValue args "--overpass-url"
                          |> Option.defaultValue defaultOverpassUrl


        let railStopsPath = argValue args "--rail-stops"
        let otherStopsPath = argValue args "--other-stops"
        let railExtSourcesDir = argValue args "--rail-ext-sources"
        let otherExtSourcesDir = argValue args "--other-ext-sources"
        let cacheDir =
            match optArgValue args "--cache-dir" with
            | Some "" -> None
            | None -> Some "/tmp"
            | cd -> cd

        let dbConnStrMod = (argValue args "--connstr") + ";CommandTimeout=0"
        let conn = getPostgresqlConnection dbConnStrMod
        conn.Open()
        cleanAndSetSchema conn "georeport"
        initSqlTables conn

        loadStopListsToSql conn railStopsPath otherStopsPath
        loadOsmDataToSql conn overpassUrl cacheDir
        loadExternalDataToSql conn railExtSourcesDir otherExtSourcesDir

        let railStopMatches = getRailStopsMatches conn
        let otherStopMatches = getOtherStopsMatches conn

        let page = resultPage railStopMatches otherStopMatches
        let html = renderHtmlDocument page
        printfn "%s" html

        0
    )
