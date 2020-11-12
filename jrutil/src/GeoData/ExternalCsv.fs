// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.GeoData.ExternalCsv

open System
open System.IO

open FSharp.Data

open JrUtil.SqlRecordStore

#nowarn "0058"

type OtherStops = CsvProvider<
    HasHeaders = false,
    Schema = "name(string), lat(float), lon(float)">
type CzRailStops = CsvProvider<
    HasHeaders = false,
    Schema = "sr70(int), name(string), lat(float), lon(float)">

let loadCzRailStopsToSql conn file =
    let stops = CzRailStops.Load(Path.GetFullPath(file))

    sqlCopyInText conn "czptt_stops_geodata"
        [| false; true; false; false; false |]
        (stops.Rows
         |> Seq.map (fun stop -> [|
             stop.Name
             string stop.Sr70
             string stop.Lat
             string stop.Lon
             "external"
         |]))

let loadOtherStopsToSql conn file =
    let stops = OtherStops.Load(Path.GetFullPath(file))

    sqlCopyInText conn "other_stops_geodata"
        [| false; false; false; false |]
        (stops.Rows
         |> Seq.map (fun stop -> [|
             stop.Name
             string stop.Lat
             string stop.Lon
             "external"
         |]))
