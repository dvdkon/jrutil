// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.GeoData.Osm

open System
open System.IO
open System.Text
open System.Threading
open System.Security.Cryptography

open FSharp.Data

open JrUtil.SqlRecordStore
open JrUtil.Utils

let czechRepBBox = [| 48.195; 12.000; 51.385; 18.951 |]
let czechRepBBoxStr = String.Join(',', czechRepBBox)

type OtherStops = JsonProvider<const(__SOURCE_DIRECTORY__ + "/../../samples/osm_other_stops.json")>
type CzRailStops = JsonProvider<const(__SOURCE_DIRECTORY__ + "/../../samples/osm_railway_stops.json")>

// cacheDir = "" -- no caching
let queryOverpass overpassUrl cacheDir (query: string) =
    let rec sendRequest () =
        let resp =
            Http.Request(
                overpassUrl,
                httpMethod = "POST",
                body = FormValues ["data", query],
                timeout = 3600000,
                responseEncodingOverride = "UTF-8",
                silentHttpErrors = true)
        // We're rate limited, wait a while and try again
        if resp.StatusCode = 429 then
            Thread.Sleep(60000)
            sendRequest()
        else
            match resp.Body with
            | Text(str) -> str
            | _ -> failwith "Overpass request returned non-string body"
    match cacheDir with
    | "" -> sendRequest()
    | cd ->
        let queryBytes = Encoding.Default.GetBytes(query)
        let queryHash =
            Convert.ToBase64String(SHA512.Create()
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
    queryOverpass overpassUrl cacheDir ("[bbox:" + czechRepBBoxStr + """]
        [out:json][timeout:100];
        ( area["ISO3166-1"="CZ"][admin_level=2]; )->.cz;
        node["railway"~"halt|station"][!"subway"](area.cz);
        out;
    """)
    |> CzRailStops.Parse
    |> (fun rs -> rs.Elements)

let osmNormalisedSr70 (stop: CzRailStops.Element) =
    stop.Tags.RefSr70
    |> Option.orElse stop.Tags.RailwayRef
    |> Option.map (fun x -> x.ToString() |> normaliseSr70)
    |> Option.defaultValue ""

let czRailStopName (stop: CzRailStops.Element) =
    stop.Tags.Name
    |> Option.orElse stop.Tags.NameCs
    |> Option.defaultValue "" // TODO: Log stops without name

let getCzOtherStops overpassUrl cacheDir =
    let subdivisions = 4;
    let latStep = (czechRepBBox.[2] - czechRepBBox.[0]) / (float subdivisions)
    let lonStep = (czechRepBBox.[3] - czechRepBBox.[1]) / (float subdivisions)
    seq {
        for i in 0 .. subdivisions do
        for j in 0 .. subdivisions do
            let bbox = String.Join(',', [|
                czechRepBBox.[0] + ((float i) * latStep)
                czechRepBBox.[1] + ((float j) * lonStep)
                czechRepBBox.[0] + ((float (i + 1)) * latStep)
                czechRepBBox.[1] + ((float (j + 1)) * lonStep)
            |])
            queryOverpass overpassUrl cacheDir ("[bbox:" + bbox + """]
                [out:json][timeout:10000][maxsize:1073741824];
                ( area["ISO3166-1"="CZ"][admin_level=2]; )->.cz;
                (
                    node(area.cz)[highway=bus_stop];
                    node(area.cz)[public_transport=platform][!train];
                    node(area.cz)[public_transport=pole];
                    node(area.cz)[railway=tram_stop];
                    node(area.cz)[public_transport=station][!train];
                    node(area.cz)[amenity=bus_station];
                );
                out;
            """)
            |> OtherStops.Parse
            |> (fun os -> os.Elements)
    }
    |> Seq.concat
