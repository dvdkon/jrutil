// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.CzPtt

open System
open System.IO
open System.Globalization
open FSharp.Data
open NodaTime

open JrUtil.Utils
open JrUtil.UnionCodec
open JrUtil.GtfsModel
open JrUtil

// TODO: Unused data:
// Some TrainActivities (Doesn't wait for connection...)

// Note that some files will error out. This is fine, as long as they don't
// describe an actual train. Some files seem to be "broken" (TODO: Why?)

exception CzPttInvalidException of msg: string
with override this.Message = this.msg

type CzPttXml = XmlProvider<Schema=const(__SOURCE_DIRECTORY__ + "/czptt.xsd")>

// TODO: Handle unknown cases in enum?
type LocationType =
    | [<StrValue("01")>] Origin
    | [<StrValue("02")>] Intermediate
    | [<StrValue("03")>] Destination
    | [<StrValue("04")>] Handover
    | [<StrValue("05")>] Interchange
    | [<StrValue("06")>] HandoverAndInterchange
    | [<StrValue("07")>] StateBorder

type TrainActivity =
    | [<StrValue("0001")>] Stops
    | [<StrValue("0026")>] CustomsStop
    | [<StrValue("0027")>] OtherStop
    | [<StrValue("0028")>] EmbarkOnly
    | [<StrValue("0029")>] DisembarkOnly
    | [<StrValue("0030")>] RequestStop
    | [<StrValue("0031")>] DepartsAtArrivalTime
    | [<StrValue("0032")>] DepartsAfterDisembark
    | [<StrValue("0033")>] NoConnectionWait
    | [<StrValue("0035")>] Preheating
    | [<StrValue("0040")>] PassWithoutStop
    // XXX: Not yet in data
    | [<StrValue("0043")>] JoinedTrains
    | [<StrValue("0044")>] Connection
    | [<StrValue("CZ01")>] StopsAfterStationOpened
    | [<StrValue("CZ02")>] WaitsLessThanHalfMinute
    // TODO: Will these ever be in static data?
    | [<StrValue("CZ03")>] HandicappedEmbark
    | [<StrValue("CZ04")>] HandicappedDisembark
    | [<StrValue("CZ05")>] WaitForDelayed
    | [<StrValue("0002")>] InternalStop
    | [<StrValue("CZ13")>] UnpublishedStop

// Traffic types could be a union, but that would make creating train names
// harder. (Each case would need to have two string values - the one used
// in data and the one used for display)
let trafficTypes =
    Map [
        ("11", "Os")
        ("C1", "Ex")
        ("C2", "R")
        ("C3", "Sp")
        ("C4", "Sv")
    ]

let commercialTrafficTypes =
    Map [
        // Some of these do not have "trademarkish" non-translateable names in
        // the official docs ("Fast train" for "R"). These are almost never
        // written like that anywhere, so the abbreviation is used for the
        // "long name" as well.
        ("50", ("EuroCity", "EC"))
        ("63", ("Intercity", "IC"))
        ("69", ("Express", "Ex"))
        ("70", ("Euro Night", "EN"))
        ("84", ("Os", "Os"))
        ("94", ("SuperCity", "SC"))
        ("122", ("Sp", "Sp"))
        ("157", ("R", "R"))
        ("209", ("RailJet", "rj"))
        ("9000", ("Rex", "Rx"))
        ("9001", ("Trilex-expres", "TLX"))
        ("9002", ("Trilex", "TL"))
        ("9003", ("LEO Expres", "LE"))
        ("9004", ("Regiojet", "RJ"))
        ("9005", ("Arriva Expres", "AEx"))
        ("9006", ("Leo Expres Tenders", "LET"))
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
            | "Sv" -> "100" // Railway Service (Sv -- maintenance train)
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

let activities (loc: CzPttXml.CzpttLocation) =
    loc.TrainActivities
    |> Seq.map ((fun ta -> ta.TrainActivityType) >> parseUnion<TrainActivity>)

let isValidGtfsStop (loc: CzPttXml.CzpttLocation) =
    // Only use stops with some activity in GTFS and without internal activities
    // XXX: Maybe it might be useful to include all stops in some cases?
    loc.TrainActivities.Length > 0
    && not (activities loc |> Seq.contains InternalStop)
    && not (activities loc |> Seq.contains UnpublishedStop)

let networkSpecificParams (czptt: CzPttXml.CzpttcisMessage) =
    czptt.NetworkSpecificParameter
    |> Option.map (fun nsps ->
        Seq.zip nsps.Names nsps.Values |> Map)
    |> Option.defaultValue (Map [])

let gtfsRoute (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation

    // Information about the train is stored in each CZPTTLocation element.
    // This is not explicitly stated in the specification, but each file only
    // contains data about one trip (TODO: Verify). This means we can simply
    // take the first *valid* element and get the data from there.
    let firstLocation =
        match info.CzpttLocations |> Seq.tryFind isValidGtfsStop with
        | Some fl -> fl
        | None ->
            raise (CzPttInvalidException "Invalid CZPTT - no valid stops")

    let prefix =
        match (firstLocation.CommercialTrafficType,
               firstLocation.TrafficType) with
        | (Some ctt, _) -> commercialTrafficTypes.[ctt] |> fst
        | (_, Some tt) -> trafficTypes.[tt]
        | _ -> ""
    // Name may end up being empty, but this should never happen in a normal
    // dataset.
    let name =
        [Some prefix
         firstLocation.OperationalTrainNumber
         networkSpecificParams czptt |> Map.tryFind "CZTrainName"]
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

let gtfsStops (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    info.CzpttLocations
    |> Array.filter isValidGtfsStop
    |> Array.collect (fun loc ->
        let createStop locType station platform = {
            id = if locType = Station
                 then gtfsStationId loc
                 else gtfsStopId loc
            code = None
            name = loc.PrimaryLocationName |> Option.defaultValue ""
            description = None
            lat = None
            lon = None
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
            [| station; platform |]
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

let timingToPeriod (timing: CzPttXml.Timing) =
    // The timezone information should be dealt with by setting stops' timezones
    let timeStr = timing.Time.Split("+").[0]
    let time = parsePeriod @"hh\:mm\:ss\.fffffff" timeStr
    let dayOffset = Period.FromDays(int timing.Offset)
    time + dayOffset


let gtfsStopTimes (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    info.CzpttLocations
    |> Array.filter isValidGtfsStop
    |> Array.mapi (fun i loc ->
        let findTime name =
            loc.TimingAtLocation.Value.Timings
            |> Array.tryFind (fun t -> t.TimingQualifierCode = name)
            |> Option.map timingToPeriod

        let arrTime = findTime "ALA"
        let depTime = findTime "ALD"

        let hasActivity a = activities loc |> Seq.contains a
        let service =
            if hasActivity RequestStop
            then CoordinationWithDriver
            else RegularlyScheduled

        let stopTime: StopTime = {
            tripId = gtfsTripId czptt
            arrivalTime = arrTime |> Option.orElse depTime
            departureTime = depTime |> Option.orElse arrTime
            stopId = gtfsStopId loc
            stopSequence = i
            headsign = None
            pickupType =
                Some (if hasActivity DisembarkOnly then NoService
                      else service)
            dropoffType =
                Some (if hasActivity EmbarkOnly then NoService
                      else service)
            shapeDistTraveled = None
            timepoint = Some Exact
            stopZoneIds = None
        }
        stopTime
    )

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
            date = LocalDate.FromDateTime(date)
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
    let fromDate = LocalDate.FromDateTime(cal.ValidityPeriod.StartDateTime)
    let toDate = cal.ValidityPeriod.EndDateTime
                 |> Option.map (fun dt -> LocalDate.FromDateTime(dt))
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
    let trainIdentifier = getIdentifierByType czptt "TR"
    let agencyNum = trainIdentifier.Company
    let agency =
        KadrEnumWs.companyForEvCisloEu agencyNum
        |> Option.get
    let gtfsAgency: Agency = {
        id = Some <| gtfsAgencyId czptt
        name = agency.ObchodNazev
        url = agency.Www |> Option.defaultValue ""
        timezone = "Europe/Prague" // TODO
        lang = None
        phone = agency.Telefon
        fareUrl = None
        email = agency.Email
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
