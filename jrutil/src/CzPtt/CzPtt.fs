// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.CzPtt

open System.IO
open FSharp.Data

open JrUtil.Utils
open JrUtil.UnionCodec
open JrUtil.GtfsModel
open System
open System.Globalization
open JrUtil

// TODO: Workaround for path resolution issues!
type CzPttXml = XmlProvider<Schema=const(__SOURCE_DIRECTORY__ + "/czptt.xsd")>

type LocationType =
    | [<StrValue("01")>] Origin
    | [<StrValue("02")>] Intermediate
    | [<StrValue("03")>] Destination
    | [<StrValue("04")>] Handover
    | [<StrValue("05")>] Interchange
    | [<StrValue("06")>] HandoverAndInterchange
    | [<StrValue("07")>] StateBorder

// Traffic types could be a union, but that would make creating train names
// harder. (Each case would need to have two string values - the one used
// in data and the one used for display)
let trafficTypes =
    Map [
        ("11", "Os");
        ("C1", "Ex");
        ("C2", "R");
        ("C3", "Sp");
    ]

let commercialTrafficTypes =
    Map [
        // Some of these do not have "trademarkish" non-translateable names in
        // the official docs ("Fast train" for "R"). These are almost never
        // written like that anywhere, so the abbreviation is used for the
        // "long name" as well.
        ("50", ("EuroCity", "EC"));
        ("63", ("Intercity", "IC"));
        ("69", ("Express", "Ex"));
        ("70", ("Euro Night", "EN"));
        ("84", ("Os", "Os"));
        ("94", ("SuperCity", "SC"));
        ("122", ("Sp", "Sp"));
        ("157", ("R", "R"));
        ("209", ("RailJet", "rj"));
        ("9000", ("Rex", "Rx"));
        ("9001", ("Trilex-expres", "TLX"));
        ("9002", ("Trilex", "TL"));
        ("9003", ("LEO Expres", "LE"));
        ("9004", ("Regiojet", "RJ"));
        ("9005", ("Arriva Expres", "AEx"));
    ]

let parseFile (path: string) =
    // .Load() uses the executable as the PWD, hence the workaround
    let doc = CzPttXml.Parse(File.ReadAllText(path))
    doc.CzpttcisMessage.Value

let gtfsRouteType (loc: CzPttXml.CzpttLocation) =
    let ttAbbr = loc.TrafficType |> Option.map (fun tt -> trafficTypes.[tt])
    let cttAbbr = loc.CommercialTrafficType |> Option.map (fun ctt ->
        commercialTrafficTypes.[ctt] |> snd)
    match cttAbbr with
    | Some ctt ->
        match ctt with
        | "EC" | "IC" | "LE" | "RJ" | "AEx" -> "102" // Long Distance Trains
        | "EN" -> "105" // Sleeper Rail
        | "Ex" | "Rx" | "rj" | "TLX" | "TL"
        | "R" | "SC" -> "103" // Inter Regional Rail
        | "Os" | "Sp" -> "106" // Regional Rail
        | _ -> failwithf "Invalid state: %s" ctt
    | None ->
        match ttAbbr with
        | Some tt ->
            match tt with
            // These labels don't really say how long a train route goes,
            // but they do signify how many stops they make.
            // So even if an "Os" train can go from Prague to Ústí n.L.
            // or from Vsetín to Velké Karlovice, they'll always
            // make more stops than an equivalent "R" or "Ex" train.
            | "Os" | "Sp" -> "106" // Regional Rail
            | "R" -> "103" // Inter Regional Rail
            | "Ex" -> "102" // Long Distance Rail
            | _ -> failwith "Invalid state"
        | None -> "100" // Railway

let getIdentifierByType (czptt: CzPttXml.CzpttcisMessage) idt =
    czptt.Identifiers.PlannedTransportIdentifiers
    |> Array.find (fun ti -> ti.ObjectType = idt)

let gtfsRouteId (czptt: CzPttXml.CzpttcisMessage) =
    let trainIdentifier = getIdentifierByType czptt "TR"
    "-CZPTTR-" + trainIdentifier.Core

let gtfsAgencyId (czptt: CzPttXml.CzpttcisMessage) =
    let trainIdentifier = getIdentifierByType czptt "TR"
    let agencyNum = trainIdentifier.Company
    sprintf "-CZPTTA-%s" agencyNum

let gtfsStationId (loc: CzPttXml.CzpttLocation) =
    sprintf "-CZPTTST-%s-%s"
            loc.CountryCodeIso
            (loc.LocationPrimaryCode |> Option.get)

let gtfsTripId czptt =
    let trainIdentifier = getIdentifierByType czptt "TR"
    sprintf "CZPTTT-%s-%s" trainIdentifier.Core trainIdentifier.Variant

let gtfsRoute (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation

    // Information about the train is stored in each CZPTTLocation element.
    // This is not explicitly stated in the specification, but each file
    // only contains data about one trip. This means we can simply take the
    // first element and get the data from there.
    let firstLocation = info.CzpttLocations.[0]

    let prefix =
        match (firstLocation.CommercialTrafficType,
               firstLocation.TrafficType) with
        | (Some ctt, _) -> commercialTrafficTypes.[ctt] |> fst
        | (_, Some tt) -> trafficTypes.[tt]
        | _ -> ""
    // Name may end up being empty, but this should never happen in a normal
    // dataset.
    let name =
        [Some prefix; firstLocation.OperationalTrainNumber]
        |> List.choose id
        |> String.concat " "

    let trainIdentifier = getIdentifierByType czptt "TR"

    let route: Route = {
        id = gtfsRouteId czptt
        agencyId = Some <| gtfsAgencyId czptt
        shortName = Some name
        longName = None
        description = None
        routeType = gtfsRouteType firstLocation
        url = None
        color = None
        textColor = None
        sortOrder = None
    }

    route

let gtfsStopId (loc: CzPttXml.CzpttLocation) =
    let platform =
        loc.LocationSubsidiaryIdentification
        |> Option.map (fun lsi -> lsi.LocationSubsidiaryCode.Value)
    sprintf "-CZPTTS-%s-%s%s"
            loc.CountryCodeIso
            (loc.LocationPrimaryCode |> Option.get)
            (platform
             |> Option.map (fun p -> "-" + p)
             |> Option.defaultValue "")


let isValidGtfsStop (loc: CzPttXml.CzpttLocation) =
    // This is kind of a heuristic, since the conversion code uses .Value on
    // optional elements, this isn't perfect. Unfortunately, location elements
    // don't quite conform to the spec (as far as I can see) and also fall into
    // two categories - "full" (usable for GTFS converion) and "partial"
    // (not meant for end-user outputs, hopefully)
    match loc.LocationSubsidiaryIdentification with
    | Some lsi -> lsi.LocationSubsidiaryCode.LocationSubsidiaryTypeCode = "1"
    | None -> loc.TimingAtLocation.IsSome

let gtfsStops (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    info.CzpttLocations
    |> Array.collect (fun loc ->
        if not <| isValidGtfsStop loc then [||]
        else
            let createStop locType station platform = {
                id = if locType = Station
                     then gtfsStationId loc
                     else gtfsStopId loc
                code = None
                name = loc.PrimaryLocationName
                description = None
                lat = 0m
                lon = 0m
                // Train routes can still have zones when joined into an
                // integrated transport system, just this format doesn't have
                // them.
                zoneId = None
                url = None
                locationType = Some locType
                parentStation = station
                // TODO: It may be possible to get the timezone of a station by
                // looking at the arrival/departure times' time zone. This will
                // have to be combined with some logic that knows about daylight
                // saving time, since the time zone is stored as an offset from
                // UTC. Or we could just assign a time zone per country and not
                // care about Russia. Or maybe GTFS accepts timezone info as a
                // UTC offset?
                timezone =
                    if locType = Station then Some "Europe/Prague" else None
                // Attributes are missing from this format, unfortunately.
                wheelchairBoarding = None
                platformCode = platform
            }

            match loc.LocationSubsidiaryIdentification with
            | Some lsi ->
                let platformCode = lsi.LocationSubsidiaryCode.Value
                let station = createStop Station None None
                let platform =
                    createStop
                        Stop
                        (gtfsStationId loc |> Some)
                        (Some platformCode)
                [| station; platform |];
            | None ->
                [| createStop Stop None None |]
    )
    // Deal with stops being driven through multiple times
    |> Array.groupBy (fun s -> s.id)
    |> Array.map (fun (_, ss) -> Seq.head ss)


let gtfsTrip (czptt: CzPttXml.CzpttcisMessage) =
    let id = gtfsTripId czptt
    let trip: Trip = {
        routeId = gtfsRouteId czptt
        serviceId = id
        id = id
        headsign = None
        shortName = None
        directionId = None
        blockId = None
        shapeId = None
        wheelchairAccessible = None
        bikesAllowed = None
    }
    trip

let timingToTimeSpan (timing: CzPttXml.Timing) =
    // The timezone information should be dealt with by setting stops' timezones
    let timeStr = timing.Time.Split("+").[0]
    let time = TimeSpan.ParseExact(timeStr, @"hh\:mm\:ss\.fffffff",
                                   CultureInfo.InvariantCulture)
    let dayOffset = new TimeSpan(int timing.Offset, 0, 0, 0)
    time + dayOffset


let gtfsStopTimes (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    info.CzpttLocations |> Array.mapi (fun i loc ->
        if not <| isValidGtfsStop loc then None
        else
            let findTime name =
                loc.TimingAtLocation.Value.Timings
                |> Array.tryFind (fun t -> t.TimingQualifierCode = name)
                |> Option.map timingToTimeSpan

            let arrTime = findTime "ALA"
            let depTime = findTime "ALD"

            let stopTime: StopTime = {
                tripId = gtfsTripId czptt
                arrivalTime = arrTime |> Option.orElse depTime
                departureTime = depTime |> Option.orElse arrTime
                stopId = gtfsStopId loc
                stopSequence = i
                headsign = None
                pickupType = None
                dropoffType = None
                shapeDistTraveled = None
                timepoint = Some Exact
            }
            Some stopTime
    )
    |> Array.choose id

let gtfsCalendarExceptions (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    let cal = info.PlannedCalendar
    let fromDate = cal.ValidityPeriod.StartDateTime
    let toDate = cal.ValidityPeriod.EndDateTime
    let days = dateTimeRange fromDate (toDate |> Option.defaultValue fromDate)
    assert (days.Length = cal.BitmapDays.Length)
    days
    |> List.mapi (fun i date ->
        let calException: CalendarException = {
            id = gtfsTripId czptt
            date = date |> dateTimeToDate
            exceptionType =
                if cal.BitmapDays.[i] = '1'
                then ServiceAdded
                else ServiceRemoved
        }
        calException
    )

let gtfsFeedInfo (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    let cal = info.PlannedCalendar
    let fromDate = cal.ValidityPeriod.StartDateTime |> dateTimeToDate
    let toDate = cal.ValidityPeriod.EndDateTime |> Option.map dateTimeToDate
    let feedInfo = {
        publisherName = "JrUtil"
        publisherUrl = "https://gitlab.com/dvdkon/jrutil"
        lang = "cs"
        startDate = Some fromDate
        endDate = toDate |> Option.defaultValue fromDate |> Some
        version = None
    }
    feedInfo

let gtfsAgency (czptt: CzPttXml.CzpttcisMessage) =
    // The returned list is annoying to match to CzPtt, since it contains
    // multiple entries per "EvCisloEU", which is the only ID contained in
    // CzPtt. This can result in wildly wrong agency information in
    // the resultant GTFS.
    let companiesResp = KadrEnumWs.requestCompanyList()
    let companies =
        companiesResp
         .Body
         .SeznamSpolecnostiResponse
         .SeznamSpolecnostiResult
         .Spolecnosts
    let trainIdentifier = getIdentifierByType czptt "TR"
    let agencyNum = trainIdentifier.Company
    let agency = companies |> Array.find (fun c -> c.EvCisloEu = agencyNum)
    let gtfsAgency: Agency = {
        id = Some <| gtfsAgencyId czptt
        name = agency.ObchodNazev
        url = agency.Www |> Option.defaultValue ""
        timezone = "Europe/Prague" // TODO
        lang = None
        phone = agency.Telefon
        fareUrl = None
        email = agency.Email |> Option.defaultValue ""
    }
    gtfsAgency

let gtfsFeed (czptt: CzPttXml.CzpttcisMessage) =
    let feed: GtfsFeed = {
        agencies = [| gtfsAgency czptt |]
        stops = gtfsStops czptt
        routes = [| gtfsRoute czptt |]
        trips = [| gtfsTrip czptt |]
        stopTimes = gtfsStopTimes czptt
        calendar = None
        calendarExceptions =
            gtfsCalendarExceptions czptt |> List.toArray |> Some
        feedInfo = gtfsFeedInfo czptt |> Some
    }
    feed
