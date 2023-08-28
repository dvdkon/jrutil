// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.Osm

open System
open System.IO
open System.Text
open System.Threading
open System.Security.Cryptography

open FSharp.Data
open NetTopologySuite.Geometries

open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.Common
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfModel
open JrUtil.JdfFixups
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

let czOtherStopNameRegion =
    // Used for synonym matching
    let matcher = new StopMatcher<_>([||])
    fun (stop: OtherStops.Element) ->
        // Stops within cities often omit the city's name on the pole, so we
        // have to add it back in. We assume official_name to be the full name.
        let point =
            wgs84Factory.CreatePoint(
                Coordinate(float stop.Lon, float stop.Lat))
            |> pointWgs84ToEtrs89Ex
        match stop.Tags.OfficialName,
              stop.Tags.Name,
              czechTownByPoint () point with
        | None, None, _ -> "", None
        | Some n, _, None -> n, None
        | Some n, _, Some (_, r, _) -> n, Some r
        | None, Some n, None -> n, None
        | None, Some n, Some (tn, r, _) ->
            if matcher.nameSimilarity(
                stopNameToTokens n, stopNameToTokens tn) = 1f
            then n, Some r
            else tn + "," + n, Some r

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
                    node(area.cz)[public_transport=platform][!train][!railway];
                    node(area.cz)[public_transport=pole];
                    node(area.cz)[railway=tram_stop];
                    node(area.cz)[public_transport=station][!train][!railway];
                    node(area.cz)[amenity=bus_station];
                );
                out;
            """)
            |> OtherStops.Parse
            |> (fun os -> os.Elements)
    }
    |> Seq.concat

let czOtherStopsForJdfMatch (stops: OtherStops.Element seq) =
    stops
    |> Seq.map (fun s ->
        let point = wgs84Factory.CreatePoint(Coordinate(float s.Lon, float s.Lat))
                    |> pointWgs84ToEtrs89Ex
        let name, region = czOtherStopNameRegion s
        {
            name = name
            data = {
                country = if region.IsSome then Some "CZ" else None
                regionId = region
                point = point
                precision = StopPrecise
            }
        })
    |> Seq.toArray
