// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CzPttToGtfs

open NodaTime
open Serilog
open Serilog.Context

open JrUtil.CzPtt
open JrUtil.CzPttMerge
open JrUtil.UnionCodec
open JrUtil.Utils
open JrUtil.GtfsModel

let gtfsRouteType (loc: CzPttXml.CzpttLocation) =
    let ttAbbr =
        loc.TrafficType
        |> nullOpt
        |> Option.map (fun tt -> (KadrEnumWs.trafficTypes ()).[tt])
    let cttAbbr =
        loc.CommercialTrafficType
        |> nullOpt
        |> Option.map (fun ctt -> (KadrEnumWs.commercialTrafficTypes ()).[ctt])
    match cttAbbr with
    | Some ctt ->
        match ctt with
        | "EC" | "IC" | "LE" | "RJ" | "AEx" -> "102" // Long Distance Trains
        | "EN" | "NJ" -> "105" // Sleeper Rail
        | "Ex" | "Rx" | "rj" | "TLX" | "TL"
        | "R" | "SC" -> "103" // Inter Regional Rail
        | "Os" | "Sp" | "LET" -> "106" // Regional Rail
        | _ -> failwithf "Unknown commercial train type: %s" ctt
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
            | _ -> failwithf "Unknown train type: %s" tt
        | None -> "100" // Railway

let hasPublicLocations (czptt: CzPttXml.CzpttcisMessage) =
    czptt.CzpttInformation.CzpttLocation
    |> Seq.exists isPublicLocation
    

// Information about the train is stored in each CZPTTLocation element.
// This is not explicitly stated in the specification, but each file only
// contains data about one trip (TODO: Verify). This means we can simply
// take the first *valid* element and get the data from there.
let firstValidLocation (czptt: CzPttXml.CzpttcisMessage) =
    match czptt.CzpttInformation.CzpttLocation
          |> Seq.tryFind isPublicLocation with
    | Some fl -> fl
    | None ->
        raise (CzPttInvalidException "Invalid CZPTT - no valid stops")

let trainTypePrefix (location: CzPttXml.CzpttLocation) =
    match (nullOpt location.CommercialTrafficType,
           nullOpt location.TrafficType) with
    | (Some ctt, _) -> (KadrEnumWs.commercialTrafficTypes ()).[ctt]
    | (_, Some tt) -> (KadrEnumWs.trafficTypes ()).[tt]
    | _ -> ""

let gtfsAgencyId (czptt: CzPttXml.CzpttcisMessage) =
    let trainIdentifier = timetableIdentifier czptt CzPttXml.ObjectType.Tr
    let agencyNum = trainIdentifier.Company
    sprintf "-CZPTTA-%s" agencyNum

let gtfsRouteId (czptt: CzPttXml.CzpttcisMessage) =
    sprintf "-CZTRAINR-%s"
            (timetableIdentifier czptt CzPttXml.ObjectType.Tr
             |> identifierStr)

let gtfsTripId czptt =
    sprintf "-CZTRAINT-%s"
            (timetableIdentifier czptt CzPttXml.ObjectType.Tr
             |> identifierStr)

let gtfsStationId (loc: CzPttXml.CzpttLocation) =
    sprintf "-SR70ST-%s-%s"
            loc.Location.CountryCodeIso
            (loc.Location.LocationPrimaryCode)

let gtfsStopId (loc: CzPttXml.CzpttLocation) =
    let platform =
        loc.Location.LocationSubsidiaryIdentification
        |> nullOpt
        |> Option.map (fun lsi ->
            lsi.LocationSubsidiaryCode.Value)
    sprintf "-SR70S-%s-%s%s"
            loc.Location.CountryCodeIso
            (loc.Location.LocationPrimaryCode)
            (platform
             |> Option.map (fun p -> "-" + p)
             |> Option.defaultValue "")

let networkSpecificParams (czptt: CzPttXml.CzpttcisMessage) =
    czptt.NetworkSpecificParameter
    |> nullOpt
    |> Option.map (fun nsps ->
        Seq.zip nsps.Name nsps.Value |> Map)
    |> Option.defaultValue (Map [])

let gtfsRoute (czptt: CzPttXml.CzpttcisMessage) =
    let firstLocation = firstValidLocation czptt

    // Name may end up being empty, but this should never happen in a normal
    // dataset.
    let name =
        [Some <| trainTypePrefix firstLocation
         nullOpt firstLocation.OperationalTrainNumber
         networkSpecificParams czptt |> Map.tryFind "CZTrainName"]
        |> List.choose id
        |> String.concat " "

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

let gtfsStops (czptts: CzPttXml.CzpttcisMessage seq) =
    czptts
    |> Seq.collect (fun c -> c.CzpttInformation.CzpttLocation)
    |> Seq.filter isPublicLocation
    |> Seq.collect (fun loc ->
        let createStop locType station platform = {
            id = if locType = Station
                 then gtfsStationId loc
                 else gtfsStopId loc
            code = None
            name = loc.Location.PrimaryLocationName
                   |> nullOpt
                   |> Option.defaultValue ""
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

        match loc.Location.LocationSubsidiaryIdentification with
        | null ->
            [| createStop Stop None None |]
        | lsi ->
            let platformCode = lsi.LocationSubsidiaryCode.Value
            let station = createStop Station None None
            let platform =
                createStop
                    Stop
                    (gtfsStationId loc |> Some)
                    (Some platformCode)
            [| station; platform |]
    )
    // Take only one (random) entry for each stop
    |> Seq.groupBy (fun s -> s.id)
    |> Seq.map (fun (_, ss) -> Seq.head ss)

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
    let time = LocalDateTime.FromDateTime(timing.Time).TimeOfDay
    let dayOffset = Period.FromDays(int timing.Offset)
    (time - LocalTime.Midnight) + dayOffset


let gtfsStopTimes (czptt: CzPttXml.CzpttcisMessage) =
    let info = czptt.CzpttInformation
    info.CzpttLocation
    |> Array.filter isPublicLocation
    |> Array.mapi (fun i loc ->
        let findTime name =
            loc.TimingAtLocation.Timing
            |> Array.tryFind (fun t -> t.TimingQualifierCode = name)
            |> Option.map timingToPeriod

        let arrTime = findTime CzPttXml.TimingTimingQualifierCode.Ala
        let depTime = findTime CzPttXml.TimingTimingQualifierCode.Ald

        let hasActivity a = locationActivities loc |> Seq.contains a
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
    let toDate = nullableOpt cal.ValidityPeriod.EndDateTime
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

let gtfsFeedInfo (czptts: CzPttXml.CzpttcisMessage seq) =
    let dates =
        czptts
        |> Seq.map (fun c ->
            let cal = c.CzpttInformation.PlannedCalendar
            let fromDate = LocalDate.FromDateTime(cal.ValidityPeriod.StartDateTime)
            let toDate = cal.ValidityPeriod.EndDateTime
                         |> nullableOpt
                         |> Option.map (fun dt -> LocalDate.FromDateTime(dt))
            fromDate, toDate)
        |> Seq.cache
    let fromDate = dates |> Seq.map fst |> Seq.min
    let toDate = dates |> Seq.map snd |> Seq.max
    let feedInfo = {
        publisherName = "JrUtil"
        publisherUrl = "https://gitlab.com/dvdkon/jrutil"
        lang = "cs"
        startDate = Some fromDate
        endDate = toDate |> Option.defaultValue fromDate |> Some
        version = None
    }
    feedInfo

let gtfsAgencies (czptts: CzPttXml.CzpttcisMessage seq) =
    czptts
    |> Seq.map (fun czptt ->
        let trainIdentifier = timetableIdentifier czptt CzPttXml.ObjectType.Tr
        trainIdentifier.Company)
    |> Set
    |> Seq.map (fun agencyNum ->
        let agency =
            KadrEnumWs.companyForEvCisloEu agencyNum
            |> Option.get
        {
            id = Some <| agencyNum
            name = agency.ObchodNazev
            url = nullOpt agency.WWW
            timezone = "Europe/Prague" // TODO
            lang = None
            phone = nullOpt agency.Telefon
            fareUrl = None
            email = nullOpt agency.Email
        })
    |> Seq.toArray

let gtfsFeed (czptts: CzPttXml.CzpttcisMessage seq) =
    let publicMessages =
        czptts |> Seq.filter hasPublicLocations
    let feed: GtfsFeed = {
        agencies = gtfsAgencies publicMessages
        stops = gtfsStops publicMessages |> Seq.toArray
        routes = publicMessages |> Seq.map gtfsRoute |> Seq.toArray
        trips = publicMessages |> Seq.map gtfsTrip |> Seq.toArray
        stopTimes = publicMessages |> Seq.collect gtfsStopTimes |> Seq.toArray
        calendar = None
        calendarExceptions =
            publicMessages
            |> Seq.collect gtfsCalendarExceptions
            |> Seq.toArray
            |> Some
        feedInfo = gtfsFeedInfo publicMessages |> Some
    }
    feed

let gtfsFeedMerged (msgs: (string * CzpttMessage) seq) =
    let merger = CzPttMerger()
    // We need to process the timetables first, before we can cancel them
    let msgs =
        msgs
        |> Seq.sortBy (fun (_, m) ->
            match m with Timetable _ -> 0 | Cancellation _ -> 1)
    for name, msg in msgs do
        use _logCtx = LogContext.PushProperty("CzPttFile", name)
        Log.Information("Merging CZPTT file {CzPttFile}", name)
        try
            merger.Process(msg)
        with
        | e -> Log.Error(e, "Error while merging {CzPttFile}", name)

    gtfsFeed merger.Messages.Values
