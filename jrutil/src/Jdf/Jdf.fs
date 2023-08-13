// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.Jdf

open JrUtil.Utils
open JrUtil.JdfParser
open JrUtil.JdfModel
open JrUtil.Jdf110To111
open JrUtil.Jdf109To110
open System
open System.IO
open System.Text.RegularExpressions

let parseAttributes batch (attributes: int option array) =
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
    let zones =
        batch.routeStops
        |> Array.filter (fun rs -> rs.stopId = stop.id)
        |> Array.map (fun rs -> rs.zone)
        |> Array.choose id
    match zones with
    | [||] -> None
    | _ -> Some <| String.concat "," zones

let jdfBatchDirVersion path =
    let versionFile =
        tryReadJdfText path "VerzeJDF.txt"
        |> Option.get
    (Regex("^\"([0-9.]+)\"[;,]")).Match(versionFile.Trim()).Groups.[1].Value

let fileParserTry name =
    // TODO: Is putting the parser in a variable better for performance?
    let parser = getJdfParser
    fun (path: string) ->
        tryReadJdfText path name
        |> Option.map parser
let fileParser name =
    let inner = fileParserTry name
    fun path -> inner path |> Option.get
let fileParserOrEmpty name =
    let inner = fileParserTry name
    fun path -> inner path |> Option.defaultValue [||]

let jdf111BatchDirParser () =
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
    let serviceNotesParser = fileParser "Caskody.txt"
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
            serviceNotes = serviceNotesParser path
            transfers = transfersParser path
            agencyAlternations = agencyAlternationsParser path
            alternateRouteNames = alternateRouteNamesParser path
            reservationOptions = reservationOptionsParser path
        }

// This repetition is annoying. Refactoring suggestions welcome.
let jdf110BatchDirParser () =
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
    let serviceNotesParser = fileParser "Caskody.txt"
    let transfersParser = fileParserOrEmpty "Navaznosti.txt"
    let agencyAlternationsParser = fileParserOrEmpty "Altdop.txt"
    let alternateRouteNamesParser = fileParserOrEmpty "Altlinky.txt"
    let reservationOptionsParser = fileParserOrEmpty "Mistenky.txt"
    fun path ->
        let batch: Jdf110Model.JdfBatch = {
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
            serviceNotes = serviceNotesParser path
            transfers = transfersParser path
            agencyAlternations = agencyAlternationsParser path
            alternateRouteNames = alternateRouteNamesParser path
            reservationOptions = reservationOptionsParser path
        }
        batch

let jdf109BatchDirParser () =
    let versionParser = fileParser "VerzeJDF.txt"
    let stopsParser = fileParser "Zastavky.txt"
    let agenciesParser = fileParser "Dopravci.txt"
    let routesParser = fileParser "Linky.txt"
    let routeStopsParser = fileParser "Zaslinky.txt"
    let tripsParser = fileParser "Spoje.txt"
    let tripStopsParser = fileParser "Zasspoje.txt"
    let routeInfoParser = fileParserOrEmpty "Udaje.txt"
    let attributeRefsParser = fileParser "Pevnykod.txt"
    let serviceNotesParser = fileParser "Caskody.txt"
    let agencyAlternationsParser = fileParserOrEmpty "Altdop.txt"
    let alternateRouteNamesParser = fileParserOrEmpty "Altlinky.txt"
    let reservationOptionsParser = fileParserOrEmpty "Mistenky.txt"
    fun path ->
        let batch: Jdf109Model.JdfBatch = {
            version = (versionParser path).[0]
            stops = stopsParser path
            agencies = agenciesParser path
            routes = routesParser path
            routeStops = routeStopsParser path
            trips = tripsParser path
            tripStops = tripStopsParser path
            routeInfo = routeInfoParser path
            attributeRefs = attributeRefsParser path
            serviceNotes = serviceNotesParser path
            agencyAlternations = agencyAlternationsParser path
            alternateRouteNames = alternateRouteNamesParser path
            reservationOptions = reservationOptionsParser path
        }
        batch

let jdfBatchDirParser () =
    let jdf111Parser = jdf111BatchDirParser()
    let jdf110Parser = jdf110BatchDirParser()
    let jdf109Parser = jdf109BatchDirParser()
    let jdf110To111Conv = jdf110To111Converter()
    let jdf109To110Conv = jdf109To110Converter()
    fun path ->
        match jdfBatchDirVersion path with
        | "1.11" -> jdf111Parser path
        | "1.10" -> jdf110Parser path |> jdf110To111Conv
        // There were only minor changes from 1.8 to 1.9
        | "1.9" | "1.8" ->
            jdf109Parser path
            |> jdf109To110Conv
            |> jdf110To111Conv
        | v -> failwithf "Unknown JDF version: \"%s\"" v

let rec findJdfBatches (path: string) =
    if Path.GetFileName(path).ToLower() = "verzejdf.txt" then
        [Path.GetDirectoryName(path)]
    else if Path.GetExtension(path).ToLower() = ".zip" then
        [path]
    else if Directory.Exists(path) then
        Seq.concat [Directory.EnumerateFiles(path);
                    Directory.EnumerateDirectories(path)]
        |> Seq.collect findJdfBatches
        |> Seq.toList
    else []
