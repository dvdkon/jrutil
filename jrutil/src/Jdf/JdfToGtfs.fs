// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.JdfToGtfs

open System
open System.Text.RegularExpressions
open NodaTime
open Serilog

open JrUtil
open JrUtil
open JrUtil.Holidays

// Information that is lost in the conversion:
// Textual notes about routes
// "Označení časového kódu" - attributes stored in ServiceNote
// Details of accessibility attributes
// Transfer attributes
// "Stop exclusivity" attributes (can't go A->C, or C->B, but B->C is fine)
// TripGroups
// And probably even more things. These are just the ones that are likely
// to become a problem.

let jdfAgencyId id idDistinction =
    sprintf "JDFA-%s-%d" id idDistinction

let jdfStopId cis id =
    if cis
    // Stop IDs which are global (from the CIS database)
    then sprintf "-CISS-%d" id
    // Stop IDs which are local to the file
    else sprintf "JDFS-%d" id

let jdfStopPostId cis stopId stopPostId =
    if cis
    then sprintf "-CISS-%d-%d" stopId stopPostId
    else sprintf "JDFS-%d-%d" stopId stopPostId

let jdfRouteId id idDistinction =
    sprintf "-CISR-%s-%d" id idDistinction

let jdfTripId routeId routeDistinction id =
    sprintf "CIST-%s-%d-%d" routeId routeDistinction id

let getGtfsRouteType (jdfRoute: JdfModel.Route) =
    match (jdfRoute.transportMode, jdfRoute.routeType) with
    | (JdfModel.Bus, JdfModel.City)
    | (JdfModel.Bus, JdfModel.CityAndAdjacent) -> "704" // Local bus
    | (JdfModel.Bus, JdfModel.International)
    | (JdfModel.Bus, JdfModel.InternationalNoNational) // International coach
    | (JdfModel.Bus, JdfModel.InternationalOrNational) -> "201"
    | (JdfModel.Bus, JdfModel.ExtraDistrict)
    | (JdfModel.Bus, JdfModel.Regional) -> "701" // Regional bus
    | (JdfModel.Bus, JdfModel.ExtraRegional)
    | (JdfModel.Bus, JdfModel.LongDistanceNational) -> "202" // National coach
    | (JdfModel.Tram, _) -> "900" // Tram
    | (JdfModel.CableCar, _) -> "1701" // Cable car
    | (JdfModel.Metro, _) -> "401" // Metro (TODO?)
    | (JdfModel.Ferry, _) -> "1000" // Water transport
    | (JdfModel.Trolleybus, _) -> "800" // Trolleybus

let getStopName (jdfStop: JdfModel.Stop) =
    // This tries to mimic how IDOS displays these names
    match (jdfStop.district, jdfStop.nearbyPlace) with
    | (None, None) -> jdfStop.town
    | (Some d, None) -> sprintf "%s,%s" jdfStop.town d
    | (None, Some np) -> sprintf "%s,,%s" jdfStop.town np
    | (Some d, Some np) -> sprintf "%s,%s,%s" jdfStop.town d np

// TODO: Naming in this whole module
let convertToGtfsAgency: JdfModel.Agency -> GtfsModel.Agency = fun jdfAgency ->
    {
        // Unfortunately in JDF most primary keys are split
        // between two fields, so we just concatenate them
        id = Some (jdfAgencyId jdfAgency.id jdfAgency.idDistinction)
        name = jdfAgency.name
        url =
            jdfAgency.website
            |> Option.map (fun url ->
                if not <| Regex.IsMatch(url, @"https?://")
                then "http://" + url
                else url
            )
            |> Option.defaultValue ""
        // This will have to be adjusted for slovak datasets
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = Some jdfAgency.officePhoneNum
        fareUrl = None
        email = jdfAgency.email
    }

let getGtfsStops stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    let gtfsStops =
        jdfBatch.stops |> Array.map (fun jdfStop ->
        {
            id = jdfStopId stopIdsCis jdfStop.id
            code = None
            name = getStopName jdfStop
            description = None
            lat = None
            lon = None
            // This is just a list of all zones this stop is in
            zoneId = Jdf.stopZone jdfBatch jdfStop
            url = None
            locationType = Some GtfsModel.Stop
            parentStation = None
            // TODO: Try to guess from jdfStop.country
            timezone = Some "Europe/Prague"

            wheelchairBoarding =
                // This doesn't use "2" ("not possible"), because there's no
                // corresponding JDF attribute
                if jdfStop.attributes
                   |> Jdf.parseAttributes jdfBatch
                   |> Set.contains JdfModel.WheelchairAccessible
                then Some 1
                else Some 0

            platformCode = None

            // TODO: Think of the best way to convey other JDF attributes
            // A column of 0/1 for each or a "set of strings" column?
        }: GtfsModel.Stop)
    let gtfsStopsById = Map <| seq { for s in gtfsStops -> s.id, s }
    let gtfsStopPosts =
        jdfBatch.stopPosts |> Array.map (fun jdfStopPost ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis jdfStopPost.stopId]
            { parentStop with

                id = jdfStopPostId stopIdsCis
                                   jdfStopPost.stopId
                                   jdfStopPost.stopPostId
                platformCode =
                    jdfStopPost.postName
                    |> Option.orElse (Some (string jdfStopPost.stopPostId))
            }: GtfsModel.Stop)

    Array.concat [ gtfsStops; gtfsStopPosts ]

let getGtfsRoutes: JdfModel.JdfBatch -> GtfsModel.Route array = fun jdfBatch ->
    jdfBatch.routes
    |> Array.map (fun jdfRoute ->
    {
        id = jdfRouteId jdfRoute.id jdfRoute.idDistinction
        agencyId = Some (jdfAgencyId jdfRoute.agencyId
                                     jdfRoute.agencyDistinction)
        // TODO: From JdfModel.RouteIntegration? What about multiple names?
        shortName = Some (string jdfRoute.id)
        longName = Some jdfRoute.name
        description = None
        // TODO: Deal with routes that don't allow national service
        // (only international)
        routeType = getGtfsRouteType jdfRoute
        url = None
        color = None
        textColor = None
        sortOrder = None
    })

let tripDateRanges (route: JdfModel.Route)
                   (tripServiceNotes: JdfModel.ServiceNote seq) =
    let tripServiceInfos = // TODO: Use hash table for lookup?
        tripServiceNotes
        |> Seq.filter (fun rt -> rt.noteType = Some JdfModel.Service)
        |> Seq.toList
    match tripServiceInfos with
    // No "Service" entry, just use full timetable validity
    | [] ->
        [DateInterval(route.timetableValidFrom, route.timetableValidTo)]
    | tsi ->
        tsi |> List.map (fun rt ->
            DateInterval(
                rt.dateFrom.Value,
                rt.dateTo |> Option.defaultValue rt.dateFrom.Value))

let dateRangesHoles (ranges: DateInterval list) =
    ranges
    |> List.sortBy (fun r -> r.Start)
    |> List.pairwise
    |> List.filter (fun (a, b) -> a.Intersection(b) = null
                                  && a.End + Period.FromDays(1) <> b.Start)
    |> List.map (fun (a, b) -> DateInterval(a.End + Period.FromDays(1),
                                            b.Start - Period.FromDays(1)))

let getGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    let tripsByRoute =
        jdfBatch.trips
        |> Array.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Map
        
    let notesByTrip =
        jdfBatch.serviceNotes
        |> Array.groupBy (fun sn -> sn.routeId, sn.routeDistinction, sn.tripId)
        |> Map

    jdfBatch.routes
    |> Array.collect (fun jdfRoute ->
        tripsByRoute
        |> Map.tryFind (jdfRoute.id, jdfRoute.idDistinction)
        |> Option.defaultWith (fun () ->
            Log.Warning("No trips for JDF route {Id},{Dist}",
                        jdfRoute.id, jdfRoute.idDistinction)
            [||])
        |> Array.map (fun jdfTrip ->
            let jdfNotes =
                notesByTrip
                |> Map.tryFind
                    (jdfRoute.id, jdfRoute.idDistinction, jdfTrip.id)
                |> Option.defaultValue [||]

            let dateRanges = tripDateRanges jdfRoute jdfNotes
            let fromDate = dateRanges |> Seq.map (fun r -> r.Start) |> Seq.min
            let toDate = dateRanges |> Seq.map (fun r -> r.End) |> Seq.max

            let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes
            let servicedDays =
                attrs
                |> Set.toList
                |> List.collect (fun a ->
                    match a with
                    // Holidays are handled in getGtfsCalendarExceptions
                    | JdfModel.WeekdayService -> [1; 2; 3; 4; 5]
                    | JdfModel.HolidaySundayService -> [7]
                    | JdfModel.DayOfWeekService(d) -> [d]
                    | _ -> []
                )

            let serviceOnlyEntryExists =
                jdfNotes
                |> Array.exists
                    (fun rt -> rt.noteType = Some JdfModel.ServiceOnly)
            let weekdays =
                if servicedDays.Length = 0
                then [| for i in [1..7] -> not serviceOnlyEntryExists |]
                else [| for i in [1..7] -> servicedDays
                                           |> List.contains i |]

            let calendarEntry: GtfsModel.CalendarEntry = {
                id = jdfTripId jdfTrip.routeId
                               jdfTrip.routeDistinction
                               jdfTrip.id
                weekdayService = weekdays
                startDate = fromDate
                endDate = toDate
            }
            calendarEntry
        )
    )


let getGtfsCalendarExceptions:
        JdfModel.JdfBatch -> GtfsModel.CalendarException array =
    let jdfDateRange (startDate: LocalDate option) endDate =
        assert startDate.IsSome
        let endDate =
            match endDate with
            | Some ed -> ed
            | None -> startDate.Value
        Utils.dateRange startDate.Value endDate
    let designationNumRegex = Regex(@"\d\d")
    fun jdfBatch ->
        let routeById =
            jdfBatch.routes
            |> Array.map (fun r -> (r.id, r.idDistinction), r)
            |> Map

        let notesByTrip =
            jdfBatch.serviceNotes
            |> Array.groupBy (fun sn -> sn.routeId, sn.routeDistinction, sn.tripId)
            |> Map

        jdfBatch.trips
        |> Array.collect (fun jdfTrip ->
            let route = routeById.[jdfTrip.routeId, jdfTrip.routeDistinction]
            let jdfNotesAll =
                notesByTrip
                |> Map.tryFind
                    (jdfTrip.routeId, jdfTrip.routeDistinction, jdfTrip.id)
                |> Option.defaultValue [||]
            let dateRanges =
                tripDateRanges route jdfNotesAll
            let fromDate = dateRanges |> Seq.map (fun r -> r.Start) |> Seq.min
            let toDate = dateRanges |> Seq.map (fun r -> r.End) |> Seq.max
            let holeDates =
                dateRangesHoles dateRanges
                |> List.collect (fun h -> Utils.dateRange h.Start h.End)

            let jdfNotes =
                jdfNotesAll
                |> Seq.filter (fun n ->
                    designationNumRegex.IsMatch(n.designation))
                // Pre-process for ease of use
                |> Seq.map (fun n -> {
                    n with
                        dateTo = n.dateTo |> Option.orElse n.dateFrom
                })
            let serviceOnlyNoteExists =
                jdfNotes
                |> Seq.exists
                    (fun rt -> rt.noteType = Some JdfModel.ServiceOnly)
            let negativeExceptionDates =
                jdfNotes
                |> Seq.filter (fun je -> je.noteType = Some JdfModel.NoService)
                |> Seq.collect (fun je ->
                    Utils.dateRange je.dateFrom.Value je.dateTo.Value)
                // When "service only" is found, getGtfsCalendar sets schedule
                // to no regular service, so we don't need to mask it out
                |> Seq.append (if serviceOnlyNoteExists then [] else holeDates)
                |> set

            let positiveExceptionDates =
                jdfNotes
                |> Seq.filter (fun je -> Seq.contains je.noteType [
                    Some JdfModel.ServiceAlso
                    Some JdfModel.ServiceOnly
                ])
                |> Seq.collect (fun je ->
                    Utils.dateRange je.dateFrom.Value je.dateTo.Value)
                |> set

            let tripAttrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes
            let holidayExcType =
                if tripAttrs |> Set.contains JdfModel.HolidaySundayService
                then Some GtfsModel.ServiceAdded
                elif tripAttrs |> Set.contains JdfModel.WeekdayService
                then Some GtfsModel.ServiceRemoved
                else None
            let years = [fromDate.Year .. toDate.Year]
            let allHolidayDates =
                years
                |> Seq.collect (fun year ->
                    czechHolidays year
                    |> Seq.map (fun (d, m) -> LocalDate(year, m, d))
                    |> Seq.filter (fun date ->
                        dateRanges |> Seq.exists (fun r -> r.Contains(date))))
                |> set
            let holidayDates =
                if holidayExcType = Some GtfsModel.ServiceAdded
                then allHolidayDates - negativeExceptionDates
                elif holidayExcType = Some GtfsModel.ServiceRemoved
                then allHolidayDates - positiveExceptionDates
                else set []

            let unsupportedNoteTypes =
                jdfNotes
                |> Seq.map (fun n -> n.noteType)
                |> set
                |> Set.filter (fun t -> not <| Seq.contains t [
                    Some JdfModel.Service
                    Some JdfModel.ServiceAlso
                    Some JdfModel.ServiceOnly
                    Some JdfModel.NoService
                    None
                ])
            for t in unsupportedNoteTypes do
                Log.Warning("Unhandled JDF service note type on route \
                             {Route}: {Type}", jdfTrip.routeId, t)

            let unsupportedDesignations =
                jdfNotes
                |> Seq.map (fun t -> t.designation)
                |> set
                |> Seq.filter (fun d -> not <| designationNumRegex.IsMatch(d))
            for d in unsupportedDesignations do
                Log.Warning("Unhandled JDF service note designation on route \
                             {Route}: {Designation}", jdfTrip.routeId, d)

            let gtfsId = jdfTripId jdfTrip.routeId jdfTrip.routeDistinction
                                   jdfTrip.id

            let positiveExceptions =
                positiveExceptionDates
                |> Set.union (
                    if holidayExcType = Some GtfsModel.ServiceAdded
                    then holidayDates
                    else set [])
                |> Seq.map (fun d -> ({
                    id = gtfsId
                    date = d
                    exceptionType = GtfsModel.ServiceAdded
                }: GtfsModel.CalendarException))

            let negativeExceptions =
                negativeExceptionDates
                |> Set.union (
                    if holidayExcType = Some GtfsModel.ServiceRemoved
                    then holidayDates
                    else set [])
                |> Seq.map (fun d -> ({
                    id = gtfsId
                    date = d
                    exceptionType = GtfsModel.ServiceRemoved
                }: GtfsModel.CalendarException))

            Seq.concat [
                positiveExceptions
                negativeExceptions
            ]
            |> Array.ofSeq
        )


let getGtfsTrips (jdfBatch: JdfModel.JdfBatch) =
    let trips: GtfsModel.Trip array =
        jdfBatch.trips
        |> Array.map (fun jdfTrip ->
        let id = jdfTripId jdfTrip.routeId jdfTrip.routeDistinction jdfTrip.id
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes
        let wheelchairAccessible =
                attrs |> Set.contains JdfModel.WheelchairAccessible
                || attrs |> Set.contains JdfModel.PartlyWheelchairAccessible
        {
            routeId = jdfRouteId jdfTrip.routeId jdfTrip.routeDistinction
            serviceId = id
            id = id
            headsign = None
            // This is kind of arbitrary
            // TODO: Customizability?
            shortName = Some (sprintf "%s %d" jdfTrip.routeId jdfTrip.id)
            directionId = Some (if jdfTrip.id % 2L = 0L then "0" else "1")
            blockId = None
            shapeId = None
            wheelchairAccessible =
                Some (if wheelchairAccessible then "1" else "0")
            bikesAllowed =
                Some (if attrs |> Set.contains JdfModel.BicycleTransport
                      then GtfsModel.OneOrMore
                      else GtfsModel.NoBicycles)
        })
    trips

let getGtfsStopTimes stopIdCis (jdfBatch: JdfModel.JdfBatch) =
    let timeToPeriod (time: LocalTime) =
        let secs = time.Hour * 3600 + time.Minute * 60 + time.Second
        Period.FromSeconds(int64 secs)
        
    let routeStopsByRoute =
        jdfBatch.routeStops
        |> Array.groupBy (fun rs -> rs.routeId, rs.routeDistinction)
        |> Map

    jdfBatch.tripStops
    // We have to deal with stop times for each trip separately,
    // because we have to count 23:59 -> 00:00 crossings
    // to even attempt to comply with GTFS and distinquish days
    // Not even this is enough, though. Imagine a trip that sets out
    // at 8:00 and, without any intermediate stops, arrives at 9:00
    // the next day.
    |> Array.groupBy
        (fun ts -> (ts.routeId, ts.routeDistinction, ts.tripId))
    |> Array.collect (fun (_, jdfTripStops) ->
        assert (jdfTripStops.Length >= 2)
        let isReverseTrip = jdfTripStops.[0].tripId % 2L = 0L

        let mutable lastTimeDT = None
        let mutable dayPeriod = Period.FromSeconds(0L)

        jdfTripStops
        |> Seq.sortBy (fun ts ->
            ts.routeStopId * (if isReverseTrip then -1L else 1L))
        |> Seq.mapi (fun i jdfTripStop ->
            let jdfRouteStop =
                routeStopsByRoute.[
                    (jdfTripStop.routeId, jdfTripStop.routeDistinction)]
                |> Array.find (fun rs ->
                    rs.routeStopId = jdfTripStop.routeStopId)

            match jdfTripStop.departureTime with
            | Some JdfModel.Passing | Some JdfModel.NotPassing -> None
            // The JDF specification allows stops that aren't served to have a
            // blank arrival and departure time. In GTFS, such stops are just
            // omitted.
            | None when jdfTripStop.arrivalTime = None -> None
            | _ ->
                let tripStopTimeExtract tst =
                    tst
                    |> Option.map (fun x ->
                        match x with
                        | JdfModel.StopTime dt -> dt
                        | _ -> failwith "Invalid data"
                    )

                let adjustTime dtOpt =
                    dtOpt |> Option.map (fun dt ->
                        match lastTimeDT with
                        | Some lt ->
                            if lt > dt
                            then dayPeriod <- dayPeriod + Period.FromDays(1)
                        | None -> ()
                        lastTimeDT <- Some dt
                        let ts = timeToPeriod dt
                        ts + dayPeriod
                    )

                let arrTime =
                    tripStopTimeExtract jdfTripStop.arrivalTime
                    |> adjustTime
                let depTime =
                    tripStopTimeExtract jdfTripStop.departureTime
                    |> adjustTime

                let jdfStop =
                    jdfBatch.stops
                    |> Array.find (fun s -> s.id = jdfTripStop.stopId)
                let stopAttrs =
                    Jdf.parseAttributes jdfBatch jdfStop.attributes
                let attrs =
                    Jdf.parseAttributes jdfBatch jdfTripStop.attributes
                let combinedAttrs = Set.union stopAttrs attrs
                let hasAttr attr = combinedAttrs |> Set.contains attr

                let service =
                    if hasAttr JdfModel.RequestStop then
                        GtfsModel.CoordinationWithDriver
                    else if hasAttr JdfModel.ConditionalService then
                    // "ConditionalService" is a very broad attribute
                    // which basically says "look at the description to
                    // find out". GTFS doesn't have such an option,
                    // and PhoneBefore implies some human interaction.
                    // so that's my choice.
                        GtfsModel.PhoneBefore
                    else if hasAttr JdfModel.CommisionServiceOnly then
                        GtfsModel.PhoneBefore
                    else
                        GtfsModel.RegularlyScheduled

                let stopTime: GtfsModel.StopTime = {
                    tripId = jdfTripId jdfTripStop.routeId
                                       jdfTripStop.routeDistinction
                                       jdfTripStop.tripId
                    arrivalTime = arrTime |> Option.orElse depTime
                    departureTime = depTime |> Option.orElse arrTime
                    stopId =
                        match jdfTripStop.stopPostId with
                        | Some sp ->
                            jdfStopPostId stopIdCis jdfTripStop.stopId sp
                        | None -> jdfStopId stopIdCis jdfTripStop.stopId
                    stopSequence = i
                    headsign = None
                    pickupType =
                        Some (if hasAttr JdfModel.ExitOnly
                              then GtfsModel.NoService
                              else service)
                    dropoffType =
                        Some (if hasAttr JdfModel.BoardingOnly
                              then GtfsModel.NoService
                              else service)
                    shapeDistTraveled = jdfTripStop.kilometer
                    // This will be dynamic when support for JDF's
                    // min/max times comes.
                    timepoint = Some GtfsModel.Exact
                    stopZoneIds = jdfRouteStop.zone
                }
                Some stopTime
        )
        |> Seq.choose id
        |> Seq.toArray
    )

// Some JDF feeds have only local IDs for stops, some have global IDs for the
// whole CIS. Set stopIdsCis accordingly.
let getGtfsFeed stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    let feed: GtfsModel.GtfsFeed = {
        agencies = jdfBatch.agencies |> Array.map convertToGtfsAgency
        stops = getGtfsStops stopIdsCis jdfBatch
        routes = getGtfsRoutes jdfBatch
        trips = getGtfsTrips jdfBatch
        stopTimes = getGtfsStopTimes stopIdsCis jdfBatch
        calendar = getGtfsCalendar jdfBatch |> Some
        calendarExceptions = getGtfsCalendarExceptions jdfBatch |> Some
        feedInfo = None
    }
    feed
