// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf

open JrUtil.JdfCsvParser
open JrUtil.JdfModel
open System.IO

let parseJdfBatchDir path =
    let parseFile name = parseCsvFile (Path.Combine(path, name))
    let parseFileOrEmpty name = let p = Path.Combine(path, name)
                                if File.Exists(p) then parseCsvFile p else [||]
    {
        version = (parseFile "VerzeJDF.txt").[0]
        stops = parseFile "Zastavky.txt"
        stopPosts = parseFileOrEmpty "Oznacniky.txt"
        agencies = parseFile "Dopravci.txt"
        routes = parseFile "Linky.txt"
        routeIntegrations = parseFileOrEmpty "LinExt.txt"
        routeStops = parseFile "Zaslinky.txt"
        trips = parseFile "Spoje.txt"
        tripGroups = parseFileOrEmpty "SpojSkup.txt"
        tripStops = parseFile "Zasspoje.txt"
        routeInfo = parseFileOrEmpty "Udaje.txt"
        attributeRefs = parseFile "Pevnykod.txt"
        routeTimes = parseFile "Caskody.txt"
        transfers = parseFileOrEmpty "Navaznosti.txt"
        agencyAlternations = parseFileOrEmpty "Altdop.txt"
        alternateRouteNames = parseFileOrEmpty "Altlinky.txt"
        reservationOptions = parseFileOrEmpty "Mistenky.txt"
    }
