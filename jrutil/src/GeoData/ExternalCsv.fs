// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

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
    Schema = "sr70(string), name(string), lat(float), lon(float)">
