// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module will load and process simplified boundaries for Czech regions
/// (okresy). It's useful for JDF processing, where regionId is part of the
/// identifier for stops.
module JrUtil.GeoData.CzRegions

open System.IO
open System.IO.Compression
open System.Text.Json
open NetTopologySuite.Geometries
open NetTopologySuite.Features
open NetTopologySuite.IO.Converters
open NetTopologySuite.Index.Strtree
open FSharp.Data
open Serilog

open JrUtil.GeoData.Common
open JrUtil.Utils

let regionsSimplifiedFile = __SOURCE_DIRECTORY__ + "/../../data/cz_regions.json.gz"
let townsWithRegionsFile = __SOURCE_DIRECTORY__ + "/../../data/cz_towns_regions.csv"
let townsFile = __SOURCE_DIRECTORY__ + "/../../data/cz_towns.json.gz"

type TownWithRegion = CsvProvider<
    HasHeaders = false,
    Schema = "id(int), name(string), regionLau1(string), regionId(string)">

// Sometimes names of towns in timetables don't match official town names,
// sometimes even multiple names are used across different timetables. This
// (hopefully forever) small list maps variations to official names.
let townSynonyms = Map [
    "Dolní Malá Úpa", "Malá Úpa"
    "Horní Malá Úpa", "Malá Úpa"
    "Podhořany u Ronova n.D.", "Podhořany u Ronova"
]

let czechRegionPolygons =
    memoizeVoidFunc <| fun () ->
        use fileStream = File.OpenRead(regionsSimplifiedFile)
        use gzStream = new GZipStream(fileStream, CompressionMode.Decompress)
        let jsonOpts = JsonSerializerOptions();
        jsonOpts.Converters.Add(GeoJsonConverterFactory(wgs84Factory));
        JsonSerializer.Deserialize<FeatureCollection>(gzStream, jsonOpts)
        |> Seq.map (fun feature ->
            feature.Attributes.["code"] :?> string,
            polygonWgs84ToEtrs89Ex (feature.Geometry :?> _))
        |> Map

let townsWithRegions =
    memoizeVoidFunc <| fun () ->
        TownWithRegion.Load(townsWithRegionsFile)

let czechTownsPolygons =
    memoizeVoidFunc <| fun () ->
        use fileStream = File.OpenRead(townsFile)
        use gzStream = new GZipStream(fileStream, CompressionMode.Decompress)
        let jsonOpts = JsonSerializerOptions();
        jsonOpts.Converters.Add(GeoJsonConverterFactory(etrs89ExFactory));
        JsonSerializer.Deserialize<FeatureCollection>(gzStream, jsonOpts)
        |> Seq.map (fun feature ->
            feature.Attributes.["name"] :?> string,
            feature.Attributes.["region"] :?> string,
            feature.Geometry :?> Polygon)
        |> Seq.toArray

let czechTownByPoint =
    memoizeVoidFunc <| fun () ->
        let index = STRtree()
        for n, r, p in czechTownsPolygons() do
            index.Insert(p.EnvelopeInternal, (n, r, p))
        fun (point: Point) ->
            let results =
                index.Query(point.EnvelopeInternal)
                |> Seq.filter (fun (_, _, p) -> p.Contains(point))
                |> Seq.toArray
            match results with
            | [| r |] -> Some r
            | [| |] -> None
            | _ ->
                Log.Error("Too many results for CZ town query! {Results}",
                          results)
                None
