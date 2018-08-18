// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf

open JrUtil.JdfCsvParser
open JrUtil.JdfModel
open System.IO
open System

let jdfBatchDirParser () =
    let fileParser name =
        let parser = getCsvFileParser
        fun path -> parser (Path.Combine(path, name))
    let fileParserOrEmpty name =
        let parser = getCsvFileParser
        fun path ->
            let p = Path.Combine(path, name)
            if File.Exists(p) then parser p else [||]

    let versionParser = fileParser "VerzeJDF.txt"
    let stopsParser = fileParser "Zastavky.txt"
    let stopPostsParser = fileParserOrEmpty "Oznacniky.txt"
    let agenciesParser = fileParser "Dopravci.txt"
    let routesParser = fileParser "Linky.txt"
    let routeIntegrationParser = fileParserOrEmpty "LinExt.txt"
    let routeStopsParser = fileParser "Zaslinky.txt"
    let tripsParser = fileParser "Spoje.txt"
    let tripGroupsParser = fileParserOrEmpty "SpojSkup.txt"
    let tripStopsParser = fileParser "Zasspoje.txt"
    let routeInfoParser = fileParserOrEmpty "Udaje.txt"
    let attributeRefsParser = fileParser "Pevnykod.txt"
    let routeTimesParser = fileParser "Caskody.txt"
    let transfersParser = fileParserOrEmpty "Navaznosti.txt"
    let agencyAlternationsParser = fileParserOrEmpty "Altdop.txt"
    let alternateRouteNamesParser = fileParserOrEmpty "Altlinky.txt"
    let reservationOptionsParser = fileParserOrEmpty "Mistenky.txt"
    fun path ->
        {
            version = (versionParser path).[0]
            stops = stopsParser path
            stopPosts = stopPostsParser path
            agencies = agenciesParser path
            routes = routesParser path
            routeIntegrations = routeIntegrationParser path
            routeStops = routeStopsParser path
            trips = tripsParser path
            tripGroups = tripGroupsParser path
            tripStops = tripStopsParser path
            routeInfo = routeInfoParser path
            attributeRefs = attributeRefsParser path
            routeTimes = routeTimesParser path
            transfers = transfersParser path
            agencyAlternations = agencyAlternationsParser path
            alternateRouteNames = alternateRouteNamesParser path
            reservationOptions = reservationOptionsParser path
        }

let parseAttributes batch attributes =
    attributes
    |> Array.choose id
    |> Set.ofArray
    |> Set.map (fun attrNum ->
        let attrRef =
            batch.attributeRefs
            |> Array.find (fun ar -> ar.attributeId = attrNum)
        attrRef.value
    )

let stopZone batch (stop: Stop) =
    // Will try to get a single zone name for a stop from routeStopslet zones
    let zones =
        batch.routeStops
        |> Array.filter (fun rs -> rs.stopId = stop.id)
        |> Array.map (fun rs -> rs.zone)
        |> Array.choose id
    if zones.Length > 0
    then let zone = zones |> Array.head
         let allSame =
             zones
             |> Array.fold (fun s z -> z = zone && s) true
         if not allSame
         then failwith "Incosistent zone information in JDF"
         else Some zone
    else None
