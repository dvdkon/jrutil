// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.ExternalCsv

open FSharp.Data
open NetTopologySuite.Geometries

open JrUtil.GeoData.Common
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfFixups

#nowarn "0058"

type OtherStops = CsvProvider<
    HasHeaders = false,
    Schema = "name(string), lat(float), lon(float), region(string option), country(string option)">
type CzRailStops = CsvProvider<
    HasHeaders = false,
    Schema = "sr70(string), name(string), lat(float), lon(float)">

let otherStopsForJdfMatch (otherStops: OtherStops) =
    otherStops.Rows
    |> Seq.map (fun s -> {
        name = s.Name
        data = {
            regionId = s.Region
            country = s.Country
            point = pointWgs84ToEtrs89Ex
                 <| wgs84Factory.CreatePoint(Coordinate(s.Lon, s.Lat))
            precision = StopPrecise
        }
    })
    |> Seq.toArray
