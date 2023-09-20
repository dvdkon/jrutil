// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.Osm

open System
open System.IO
open OsmSharp
open OsmSharp.Geo
open OsmSharp.Streams
open NetTopologySuite.Features
open NetTopologySuite.Geometries

open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.Common
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfModel
open JrUtil.JdfFixups
open JrUtil.Utils

type CzOtherStop = {
    id: int64
    name: string option
    officialName: string option
    point: Point
}

let featTagOpt name (feat: IFeature) =
    feat.Attributes.GetOptionalValue(name)
    |> nullOpt
    |> Option.map unbox<string>

let getCzRailStops pbfPath = cacheVoidFunc "cz-osm-rail-stops" <| fun () ->
    use stream = File.OpenRead(pbfPath)
    (new PBFOsmStreamSource(stream)
     |> Seq.filter (fun node ->
         node.Type = OsmGeoType.Node
         && (node.Tags.Contains("railway", "halt")
             || node.Tags.Contains("railway", "station"))
         && not (node.Tags.ContainsKey("subway"))))
     .ToFeatureSource()
    |> Seq.map (fun feat -> {|
        id = feat.Attributes.["id"] :?> int64
        sr70 =
            featTagOpt "ref:sr70" feat
            |> Option.orElse (featTagOpt "railway:ref" feat)
            |> Option.map normaliseSr70
        name =
            featTagOpt "name" feat
            |> Option.orElse (featTagOpt "name:cs" feat)
        point = wgs84Factory.CreateGeometry(feat.Geometry) :?> Point
    |})
    |> Seq.toArray

let czOtherStopNameRegion =
    // Used for synonym matching
    let matcher = new StopMatcher<_>([||])
    fun (stop: CzOtherStop) etrs89ExPt ->
        // Stops within cities often omit the city's name on the pole, so we
        // have to add it back in. We assume official_name to be the full name.
        match stop.officialName,
              stop.name,
              czechTownByPoint () etrs89ExPt with
        | None, None, _ -> "", None
        | Some n, _, None -> n, None
        | Some n, _, Some (_, r, _) -> n, Some r
        | None, Some n, None -> n, None
        | None, Some n, Some (tn, r, _) ->
            if matcher.nameSimilarity(
                stopNameToTokens n, stopNameToTokens tn) = 1f
            then n, Some r
            else tn + "," + n, Some r

let getCzOtherStops pbfPath = cacheVoidFunc "cz-osm-other-stops" <| fun () ->
    use stream = File.OpenRead(pbfPath)
    (new PBFOsmStreamSource(stream)
     |> Seq.filter (fun node ->
         node.Type = OsmGeoType.Node
         && not (node.Tags.ContainsKey("train"))
         && (not (node.Tags.ContainsKey("railway"))
             || node.Tags.ContainsKey("tram"))
         && (node.Tags.Contains("highway", "bus_stop")
             || node.Tags.Contains("public_transport", "platform")
             || node.Tags.Contains("public_transport", "pole")
             || node.Tags.Contains("public_transport", "station")
             || node.Tags.Contains("railway", "tram_stop")
             || node.Tags.Contains("amenity", "bus_station"))))
     .ToFeatureSource()
    |> Seq.map (fun feat -> {
        id = feat.Attributes.["id"] :?> int64
        name =
            featTagOpt "name" feat
            |> Option.orElse (featTagOpt "name:cs" feat)
        officialName = featTagOpt "official_name" feat
        point = wgs84Factory.CreateGeometry(feat.Geometry) :?> Point
    })
    |> Seq.toArray

let czOtherStopsForJdfMatch (stops: CzOtherStop seq) =
    stops
    |> Seq.map (fun s ->
        let etrs89ExPt = pointWgs84ToEtrs89Ex s.point
        let name, region = czOtherStopNameRegion s etrs89ExPt
        {
            name = name
            data = {
                country = if region.IsSome then Some "CZ" else None
                regionId = region
                point = etrs89ExPt
                precision = StopPrecise
            }
        })
    |> Seq.toArray
