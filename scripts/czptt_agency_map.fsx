#load "jrutil/ref.fsx"
// Shouldn't be necessary, I need to fix the script
#load "ext-services-cs/ref.fsx"
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Collections.Generic
open NetTopologySuite.Geometries
open NetTopologySuite.Features
open NetTopologySuite.IO.Converters
open JrUtil

let sr70file: string = fsi.CommandLineArgs.[2]
let stops =
    GeoData.ExternalCsv.CzRailStops.Parse(File.ReadAllText(sr70file)).Rows
let stopsBySr70 =
    stops
    |> Seq.map (fun s -> Utils.normaliseSr70 s.Sr70, s)
    |> Map

let czpttFolder = fsi.CommandLineArgs.[1]
let czptts = CzPtt.parseAll czpttFolder
let merger = CzPttMerge.CzPttMerger()
merger.ProcessAll(czptts)

let segmentAgencies = Dictionary<string * string, Dictionary<string, int>>()
for tt in merger.Messages.Values do
    let locs = tt.CzpttInformation.CzpttLocation
    if locs |> Seq.exists CzPtt.isPublicLocation |> not then ()
    else
        let firstPublic = locs |> Seq.findIndex CzPtt.isPublicLocation
        let lastPublic = locs |> Seq.findIndexBack CzPtt.isPublicLocation
        for l1, l2 in locs.[firstPublic..lastPublic] |> Seq.pairwise do
            let lids = [| l1.Location.LocationPrimaryCode
                          l2.Location.LocationPrimaryCode |]
            let segmentId = Array.min lids, Array.max lids

            if segmentAgencies.ContainsKey(segmentId) |> not then
                segmentAgencies.[segmentId] <- Dictionary()
            let counter = segmentAgencies.[segmentId]
            if counter.ContainsKey(l1.ResponsibleRu) |> not then
                counter.[l1.ResponsibleRu] <- 0
            counter.[l1.ResponsibleRu] <- counter.[l1.ResponsibleRu] + 1

let features = FeatureCollection()
for e in segmentAgencies do
    let (si1, si2), agencyCounts = e.Key, e.Value
    let agencies =
        agencyCounts
        |> Seq.map (fun kv ->
            KadrEnumWs.companyForEvCisloEu kv.Key
            |> Option.map (fun c -> c, kv.Value))
        |> Seq.choose id
    match (stopsBySr70 |> Map.tryFind (Utils.normaliseSr70 si1)),
          (stopsBySr70 |> Map.tryFind (Utils.normaliseSr70 si2)) with
        | Some s1, Some s2 ->
            let gf = GeoData.Common.wgs84Factory
            let agencyIds, agencyNames, agencyCounts =
                agencies |> Seq.map (fun (a, c) ->
                             [string a.EvCisloEU],
                             [string a.ZkrObchodNazev],
                             [c])
                         |> Seq.sort
                         |> Utils.concatTo3
            features.Add(Feature(gf.CreateLineString([|
                Coordinate(s1.Lon, s1.Lat)
                Coordinate(s2.Lon, s2.Lat)
            |]), AttributesTable([
                KeyValuePair("AgencyIDs", agencyIds |> box)
                KeyValuePair("AgencyNames", agencyNames |> box)
                KeyValuePair("AgencyCounts", agencyCounts |> box)
            ])))
        | None, _ -> eprintfn "Missing data for stop %s" si1
        | _, None -> eprintfn "Missing data for stop %s" si2

let jsonOpts = JsonSerializerOptions()
jsonOpts.Converters.Add(GeoJsonConverterFactory());
jsonOpts.NumberHandling <- JsonNumberHandling.AllowNamedFloatingPointLiterals
printfn "%s" <| JsonSerializer.Serialize(features, jsonOpts)
