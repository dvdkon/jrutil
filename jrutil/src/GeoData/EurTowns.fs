// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module references a file of town-name-to-country mapping created from
/// OSM data, specifically the osmnames.org exports
/// This data is licenced under the ODbL, see https://www.openstreetmap.org/copyright
///
/// Created by the following Python script:
///     import csv, sys, gzip, math
///     csv.field_size_limit(sys.maxsize)
///     filtered = []
///     for r in csv.reader(gzip.open("planet-latest_geonames.tsv.gz", "tr"),
///                         dialect="excel-tab"):
///         if r[5] in ["administrative", "city", "town", "village", "hamlet"] \
///             and r[15] in ["at", "be", "bg", "hr", "cy", "dk", "ee",
///                           "fi", "fr", "de", "gr", "hu", "ie", "it", "lv",
///                           "lt", "lu", "mt", "nl", "pl", "pt", "ro", "sk",
///                           "si", "es", "se", "gb", "ua", "ch", "no", "li",
///                           "ba", "me", "md", "mk", "al", "rs", "tr"]:
///             filtered.append((r[0], int(r[8]), r[15], float(r[7]), float(r[6])))
///             for n in r[1].split(","):
///                 filtered.append((n, int(r[8]) + 10, r[15], float(r[7]), float(r[6])))
///     filtered.sort(key=lambda r: r[1])
///     map_uniq = {}
///     for r in filtered:
///         n = r[0].strip()
///         if n in map_uniq:
///             c, lat, lon = map_uniq[n]
///             if lat is not None and math.sqrt((lat - r[3])**2 + (lon - r[4])**2) > 0.1:
///                 map_uniq[n] = (c, None, None)
///         else:
///             map_uniq[n] = (r[2], r[3], r[4])
///     csv.writer(open("eur_towns_countries.csv", "w")) \
///         .writerows((k, *v) for k, v in map_uniq.items())
module JrUtil.GeoData.EurTowns

open System.IO
open System.IO.Compression
open FSharp.Data

open JrUtil.Utils

let eurTownsCountriesFile = __SOURCE_DIRECTORY__ + "/../../data/eur_towns_countries.csv"

type EurTownCountry = CsvProvider<
    HasHeaders = false,
    Schema = "name(string), countryCode(string), lat(float option), lon(float option)">

let eurTownCountries =
    memoizeVoidFunc <| fun () ->
        EurTownCountry.Load(eurTownsCountriesFile).Rows
        |> Seq.toArray
