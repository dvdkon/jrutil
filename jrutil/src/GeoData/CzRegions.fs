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
open FSharp.Data

open JrUtil.GeoData.Common
open JrUtil.Utils

let regionsSimplifiedFile = __SOURCE_DIRECTORY__ + "/../../data/regions.json.gz"
let townsWithRegionsFile = __SOURCE_DIRECTORY__ + "/../../data/towns_regions.csv"

type TownWithRegion = CsvProvider<
    HasHeaders = false,
    Schema = "id(int), name(string), regionLau1(string), regionId(string)">

let czechRegionPolygons =
    memoizeVoidFunc <| fun () ->
        use fileStream = File.OpenRead(regionsSimplifiedFile)
        use gzStream = new GZipStream(fileStream, CompressionMode.Decompress)
        let jsonOpts = JsonSerializerOptions();
        let geomFactory = GeometryFactory(PrecisionModel(), 4326)
        jsonOpts.Converters.Add(GeoJsonConverterFactory(geomFactory));
        JsonSerializer.Deserialize<FeatureCollection>(gzStream, jsonOpts)
        |> Seq.map (fun feature ->
            feature.Attributes.["code"] :?> string,
            polygonWgs84ToEtrs89Ex (feature.Geometry :?> _))
        |> Map

let townsWithRegions =
    memoizeVoidFunc <| fun () ->
        TownWithRegion.Load(townsWithRegionsFile)
