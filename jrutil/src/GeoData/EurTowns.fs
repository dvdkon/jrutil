// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module references a file of town-name-to-country mapping created from
/// OSM data, specifically the osmnames.org exports
/// This data is licenced under the ODbL, see https://www.openstreetmap.org/copyright
///
/// Created by the following Python script:
///     import csv, sys, gzip
///     csv.field_size_limit(sys.maxsize)
///     filtered = []
///     for r in csv.reader(gzip.open("planet-latest_geonames.tsv.gz", "tr"), dialect="excel-tab"):
///         if r[5] in ["administrative", "city", "town", "village", "hamlet"] \
///             and r[15] in ["at", "be", "bg", "hr", "cy", "dk", "ee", 
///                           "fi", "fr", "de", "gr", "hu", "ie", "it", "lv",
///                           "lt", "lu", "mt", "nl", "pl", "pt", "ro", "sk",
///                           "si", "es", "se", "gb", "ua", "ch", "no", "li",
///                           "ba", "me", "md", "mk", "al", "rs", "tr"]:
///             filtered.append((r[0], r[1], r[8], r[15]))
///     filtered.sort(key=lambda r: r[2])
///     map_uniq = {}
///     for r in filtered:
///         for n in [r[0]] + r[1].split(","):
///             n = n.strip()
///             if n in map_uniq: continue
///             map_uniq[n] = r[3]
///     csv.writer(open("eur_towns_countries.csv", "w")).writerows(map_uniq.items())
module JrUtil.GeoData.EurTowns

open System.IO
open System.IO.Compression
open FSharp.Data

open JrUtil.Utils

let eurTownsCountriesFile = __SOURCE_DIRECTORY__ + "/../../data/eur_towns_countries.csv"

type EurTownCountry = CsvProvider<
    HasHeaders = false,
    Schema = "name(string), countryCode(string)">

let eurTownCountries =
    memoizeVoidFunc <| fun () ->
        EurTownCountry.Load(eurTownsCountriesFile).Rows
        |> Seq.toArray
