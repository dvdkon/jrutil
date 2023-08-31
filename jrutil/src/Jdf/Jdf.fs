// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.Jdf

open System.IO
open System.IO.Compression
open System.Text.RegularExpressions

open JrUtil.JdfParser
open JrUtil.JdfModel
open JrUtil.Jdf110To111
open JrUtil.Jdf109To110
open JrUtil.JdfSerializer
open JrUtil.Utils

type JdfBatchDirectory =
    | FsPath of string
    | ZipArchive of ZipArchive

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

/// name: filename inside batch (case-insensitive)
let tryReadJdfText (dir: JdfBatchDirectory) name =
    match dir with
    | FsPath path ->
        findPathCaseInsensitive path name
        |> Option.map (fun p -> File.ReadAllText(p, jdfEncoding))
    | ZipArchive arch ->
        tryParseJdfTextFromZip (fun s ->
            use reader = new StreamReader(s, jdfEncoding)
            reader.ReadToEnd()
        ) arch name

let jdfBatchDirVersion path =
    let versionFile =
        tryReadJdfText path "VerzeJDF.txt"
        |> Option.get
    (Regex("^\"([0-9.]+)\"[;,]")).Match(versionFile.Trim()).Groups.[1].Value

let fileParserTry name =
    // TODO: Is putting the parser in a variable better for performance?
    let parser: Stream -> 'a seq = getJdfParser
    fun path ->
        match path with
        | FsPath dir ->
            findPathCaseInsensitive dir name
            |> Option.map (fun fullPath ->
                use stream = File.OpenRead(fullPath)
                parser stream |> Seq.toArray)
        | ZipArchive arch ->
            tryParseJdfTextFromZip parser arch name
            |> Option.map Seq.toArray
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
    let stopLocationsParser = fileParserOrEmpty "Polzast.txt"
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
            stopLocations = stopLocationsParser path
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
        let dirPath = Path.GetDirectoryName(path)
        [dirPath, FsPath dirPath]
    else if Path.GetExtension(path).ToLower() = ".zip" then
        [path, ZipArchive <| ZipFile.OpenRead(path)]
    else if Directory.Exists(path) then
        Seq.concat [Directory.EnumerateFiles(path);
                    Directory.EnumerateDirectories(path)]
        |> Seq.collect findJdfBatches
        |> Seq.toList
    else []

let jdfStopNameString (stop: Stop) =
    sprintf "%s,%s,%s"
            stop.town
            (stop.district |> Option.defaultValue "")
            (stop.nearbyPlace |> Option.defaultValue "")

let fileWriter name =
    let serializer: Stream -> 'r seq -> unit = getJdfSerializerWriter
    fun (dir: JdfBatchDirectory) records () ->
        match dir with
        | FsPath path ->
            use stream = File.Open(Path.Combine(path, name), FileMode.Create)
            serializer stream records
        | ZipArchive arch ->
            use stream = arch.CreateEntry(name).Open()
            serializer stream records

let jdfBatchDirWriter () =
    let versionWriter = fileWriter "VerzeJDF.txt"
    let stopsWriter = fileWriter "Zastavky.txt"
    let stopPostsWriter = fileWriter "Oznacniky.txt"
    let agenciesWriter = fileWriter "Dopravci.txt"
    let routesWriter = fileWriter "Linky.txt"
    let routeIntegrationWriter = fileWriter "LinExt.txt"
    let routeStopsWriter = fileWriter "Zaslinky.txt"
    let tripsWriter = fileWriter "Spoje.txt"
    let tripGroupsWriter = fileWriter "SpojSkup.txt"
    let tripStopsWriter = fileWriter "Zasspoje.txt"
    let routeInfoWriter = fileWriter "Udaje.txt"
    let attributeRefsWriter = fileWriter "Pevnykod.txt"
    let serviceNotesWriter = fileWriter "Caskody.txt"
    let transfersWriter = fileWriter "Navaznosti.txt"
    let agencyAlternationsWriter = fileWriter "Altdop.txt"
    let alternateRouteNamesWriter = fileWriter "Altlinky.txt"
    let reservationOptionsWriter = fileWriter "Mistenky.txt"
    let stopLocationsWriter = fileWriter "Polzast.txt"

    fun dir (batch: JdfBatch) ->
        versionWriter dir [|
            { batch.version with version = "1.11" }
        |] ()
        stopsWriter dir batch.stops ()
        stopPostsWriter dir batch.stopPosts ()
        agenciesWriter dir batch.agencies ()
        routesWriter dir batch.routes ()
        routeIntegrationWriter dir batch.routeIntegrations ()
        routeStopsWriter dir batch.routeStops ()
        tripsWriter dir batch.trips ()
        tripGroupsWriter dir batch.trips ()
        tripStopsWriter dir batch.tripStops ()
        routeInfoWriter dir batch.routeInfo ()
        attributeRefsWriter dir batch.attributeRefs ()
        serviceNotesWriter dir batch.serviceNotes ()
        transfersWriter dir batch.transfers ()
        agencyAlternationsWriter dir batch.agencyAlternations ()
        alternateRouteNamesWriter dir batch.alternateRouteNames ()
        reservationOptionsWriter dir batch.reservationOptions ()
        stopLocationsWriter dir batch.stopLocations ()
