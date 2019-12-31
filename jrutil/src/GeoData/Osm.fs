// This file is part of JrUtil and is licenced under the GNU GPLv2 or later
// (c) 2019 David Koňařík

module JrUtil.GeoData.Osm

open System
open System.IO
open System.Text
open System.Security.Cryptography

open FSharp.Data

open JrUtil.SqlRecordStore

let czechRepBBox = "48.195,12.000,51.385,18.951"

type CzRailStops = JsonProvider<const(__SOURCE_DIRECTORY__ + "/../../samples/osm_railway_stops.json")>

// cacheDir = "" -- no caching
let queryOverpass overpassUrl cacheDir (query: string) =
    let sendRequest () =
        Http.RequestString(
            overpassUrl,
            httpMethod = "POST",
            body = FormValues ["data", query],
            timeout = 1000000,
            responseEncodingOverride = "UTF-8")
    match cacheDir with
    | "" -> sendRequest()
    | cd ->
        let queryBytes = Encoding.Default.GetBytes(query)
        let queryHash =
            Convert.ToBase64String((new SHA256Managed())
                                    .ComputeHash(queryBytes))
             .Replace("/", "-")
        let filename =
            "jrutil_overpass_cache_" + queryHash
        let path = Path.Combine(cd, filename)
        if File.Exists(path) then File.ReadAllText(path)
        else
            let result = sendRequest()
            File.WriteAllText(path, result)
            result

let getCzRailStops overpassUrl cacheDir =
    queryOverpass overpassUrl cacheDir ("[bbox:" + czechRepBBox + """]
        [out:json][timeout:100];
        ( area["ISO3166-1"="CZ"][admin_level=2]; )->.cz;
        node["railway"~"halt|station"][!"subway"](area.cz);
        out;
    """)
    |> CzRailStops.Parse

let normalisedSr70 (stop: CzRailStops.Element) =
    stop.Tags.RefSr70
    |> Option.orElse stop.Tags.RailwayRef
    |> Option.map (fun x ->
        let raw = x.ToString()
        // Strip off checksum digit, if present
        if raw.Length > 5 then raw.[..4]
        else raw)
    |> Option.defaultValue ""

let czRailStopName (stop: CzRailStops.Element) =
    stop.Tags.Name
    |> Option.orElse stop.Tags.NameCs
    |> Option.defaultValue "" // TODO: Log stops without name

let czRailStopsToSql conn (data: CzRailStops.Root) =
    sqlCopyInText conn "czptt_stops_geodata"
        [| false; true; false; false; false |]
        (data.Elements
         |> Seq.map (fun rso -> [|
            czRailStopName rso
            normalisedSr70 rso
            string rso.Lat
            string rso.Lon
            "osm"
         |]))
