// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfToGtfs

open System
open System.Text.RegularExpressions
open NodaTime
open NodaTime.Calendars
open Serilog

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
        // This will have to be adjusted for slovak datasets
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = Some jdfAgency.officePhoneNum
        fareUrl = None
        email = jdfAgency.email
    }

let getGtfsStops stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    let stopLocationsById =
        jdfBatch.stopLocations
        |> Seq.filter (fun sl -> sl.precision = JdfModel.StopPrecise)
        |> Seq.map (fun sl ->
            sl.stopId, (sl.lat, sl.lon))
        |> Map

    let gtfsStops =
        jdfBatch.stops |> Array.map (fun jdfStop ->
            let latLon =
                stopLocationsById
                |> Map.tryFind jdfStop.id
            {
                id = jdfStopId stopIdsCis jdfStop.id
                code = None
                name = getStopName jdfStop
                description = None
                lat = latLon |> Option.map fst
                lon = latLon |> Option.map snd
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


/// Returns a boolean array, where each item represents a day in the route's
/// validity interval, true if the trip should run, false otherwise.
let tripDateBitmap (route: JdfModel.Route)
                   (trip: JdfModel.Trip)
                   (tripServiceNotes: JdfModel.ServiceNote seq)
                   (tripAttributes: JdfModel.Attribute Set) =
    let jdfNotes =
        tripServiceNotes
        // Pre-process for ease of use
        |> Seq.choose (fun sn ->
            sn.noteType |> Option.map (fun nt ->
                let df = sn.dateFrom
                         |> Option.defaultValue route.timetableValidFrom
                {|
                    noteType = nt
                    dateFrom = df
                    dateTo = sn.dateTo |> Option.defaultValue df
                |}))
        |> Seq.toArray
    let holidays =
        czechHolidayDates route.timetableValidFrom route.timetableValidTo
        |> set
    let hasDateAttribute =
        tripAttributes |> Seq.exists (fun a ->
            match a with
            | JdfModel.WeekdayService
            | JdfModel.HolidaySundayService
            | JdfModel.DayOfWeekService _ -> true
            | _ -> false)
    Utils.dateRange route.timetableValidFrom route.timetableValidTo
    |> Seq.map (fun d ->
        let applicableNoteTypes =
            jdfNotes
            |> Seq.filter (fun sn ->
                DateInterval(sn.dateFrom, sn.dateTo).Contains(d))
            |> Seq.map (fun sn -> sn.noteType)
            |> set
        let hasNote noteType = applicableNoteTypes |> Set.contains noteType
        if hasNote JdfModel.ServiceOnly then true
        else if hasNote JdfModel.NoService then false
        else if hasNote JdfModel.ServiceAlso then true
        else if hasNote JdfModel.ServiceAlso then true
        else if hasNote JdfModel.ServiceAlso then true
        else if (hasNote JdfModel.ServiceOddWeeks
                 || hasNote JdfModel.ServiceOddWeeksFromTo)
             && WeekYearRules.Iso.GetWeekOfWeekYear(d) % 2 = 0 then false
        else if (hasNote JdfModel.ServiceEvenWeeks
                 || hasNote JdfModel.ServiceEvenWeeksFromTo)
             && WeekYearRules.Iso.GetWeekOfWeekYear(d) % 2 = 1 then false
        else if (not <| hasNote JdfModel.Service)
             && jdfNotes |> Array.exists (fun sn ->
                    sn.noteType = JdfModel.Service) then false
        else if tripAttributes |> Set.contains JdfModel.HolidaySundayService
             && (holidays |> Set.contains d
                 || d.DayOfWeek = IsoDayOfWeek.Sunday) then true
        else if tripAttributes |> Set.contains JdfModel.WeekdayService
             && holidays |> Set.contains d |> not
             && d.DayOfWeek <> IsoDayOfWeek.Saturday
             && d.DayOfWeek <> IsoDayOfWeek.Sunday then true
        else if tripAttributes
                |> Set.contains (JdfModel.DayOfWeekService (
                    LanguagePrimitives.EnumToValue d.DayOfWeek)) then true
        else not hasDateAttribute)
    |> Seq.toArray

let gtfsCalendarBitmap (calendar: GtfsModel.CalendarEntry) =
    Utils.dateRange calendar.startDate calendar.endDate
    |> Seq.map (fun d ->
        let dow = LanguagePrimitives.EnumToValue d.DayOfWeek - 1
        calendar.weekdayService.[dow])
    |> Seq.toArray

// Returns triple of trips with empty calendar (to delete), calendar entries
// and calendar exceptions
let getGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    let tripsByRoute =
        jdfBatch.trips
        |> Array.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Map

    let notesByTrip =
        jdfBatch.serviceNotes
        |> Array.groupBy (fun sn -> sn.routeId, sn.routeDistinction, sn.tripId)
        |> Map

    let routeById =
        jdfBatch.routes
        |> Array.map (fun r -> (r.id, r.idDistinction), r)
        |> Map

    jdfBatch.trips
    |> Seq.map (fun jdfTrip ->
        let jdfRoute = routeById.[(jdfTrip.routeId, jdfTrip.routeDistinction)]
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes

        let servicedDays =
            attrs
            |> Set.toList
            |> List.collect (fun a ->
                match a with
                | JdfModel.WeekdayService -> [1; 2; 3; 4; 5]
                | JdfModel.HolidaySundayService -> [7]
                | JdfModel.DayOfWeekService(d) -> [d]
                | _ -> [])
        let weekdays =
            if servicedDays.Length = 0
            then [| for i in [1..7] -> true |]
            else [| for i in [1..7] -> servicedDays
                                       |> List.contains i |]
        let tripId = jdfTripId jdfTrip.routeId
                               jdfTrip.routeDistinction
                               jdfTrip.id

        let calendarEntry: GtfsModel.CalendarEntry = {
            id = tripId
            weekdayService = weekdays
            startDate = jdfRoute.timetableValidFrom
            endDate = jdfRoute.timetableValidTo
        }

        let bitmap =
            tripDateBitmap
                jdfRoute jdfTrip
                (notesByTrip
                 |> Map.tryFind (jdfTrip.routeId,
                                 jdfTrip.routeDistinction,
                                 jdfTrip.id)
                 |> Option.defaultValue [||])
                attrs
        let calendarBitmap = gtfsCalendarBitmap calendarEntry
        let bitmapDiffCount =
            Seq.zip bitmap calendarBitmap
            |> Seq.sumBy (fun (s1, s2) -> if s1 = s2 then 0 else 1)
        let bitmapTrueCount =
            bitmap |> Array.sumBy (fun s -> if s then 1 else 0)

        // Pick the most efficient representation (calendar + exceptions vs
        // just exceptions)
        if bitmapTrueCount = 0 then
            seq { tripId }, Seq.empty, Seq.empty
        elif bitmapDiffCount > bitmapTrueCount then
            Seq.empty, Seq.empty,
            bitmap
            |> Seq.indexed
            |> Seq.choose (fun (i, s) ->
                if s then Some ({
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = GtfsModel.ServiceAdded
                }: GtfsModel.CalendarException)
                else None)
        else
            Seq.empty, seq { calendarEntry },
            Seq.zip bitmap calendarBitmap
            |> Seq.indexed
            |> Seq.choose (fun (i, (s, sc)) ->
                if s <> sc then Some {
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = if s then GtfsModel.ServiceAdded
                                    else GtfsModel.ServiceRemoved
                }
                else None)
    )
    |> Utils.concatTo3
    |> (fun (ets, ces, cexs) -> set ets, ces.ToArray(), cexs.ToArray())

let getGtfsTrips (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.trips
    |> Seq.map (fun jdfTrip ->
        let id = jdfTripId jdfTrip.routeId jdfTrip.routeDistinction jdfTrip.id
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes
        let wheelchairAccessible =
                attrs |> Set.contains JdfModel.WheelchairAccessible
                || attrs |> Set.contains JdfModel.PartlyWheelchairAccessible
        ({
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
        }: GtfsModel.Trip))

let getGtfsStopTimes stopIdCis (jdfBatch: JdfModel.JdfBatch) =
    let timeToPeriod (time: LocalTime) =
        let secs = time.Hour * 3600 + time.Minute * 60 + time.Second
        Period.FromSeconds(int64 secs)

    let routeStopsByRoute =
        jdfBatch.routeStops
        |> Array.groupBy (fun rs -> rs.routeId, rs.routeDistinction)
        |> Map

    let stopById =
        jdfBatch.stops
        |> Array.map (fun s -> s.id, s)
        |> Map

    jdfBatch.tripStops
    // We have to deal with stop times for each trip separately,
    // because we have to count 23:59 -> 00:00 crossings
    // to even attempt to comply with GTFS and distinquish days
    // Not even this is enough, though. Imagine a trip that sets out
    // at 8:00 and, without any intermediate stops, arrives at 9:00
    // the next day.
    |> Seq.groupBy
        (fun ts -> (ts.routeId, ts.routeDistinction, ts.tripId))
    |> Seq.collect (fun (_, jdfTripStops) ->
        let jdfTripStops = Seq.toArray jdfTripStops
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

                let jdfStop = stopById |> Map.find jdfTripStop.stopId
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
    )

// Some JDF feeds have only local IDs for stops, some have global IDs for the
// whole CIS. Set stopIdsCis accordingly.
let getGtfsFeed stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    let tripsToDelete, calendar, calendarExceptions = getGtfsCalendar jdfBatch
    let feed: GtfsModel.GtfsFeed = {
        agencies = jdfBatch.agencies |> Array.map convertToGtfsAgency
        stops = getGtfsStops stopIdsCis jdfBatch
        routes = getGtfsRoutes jdfBatch
        trips = getGtfsTrips jdfBatch
            |> Seq.filter (fun t ->
                tripsToDelete |> Set.contains t.id |> not)
            |> Seq.toArray
        stopTimes = getGtfsStopTimes stopIdsCis jdfBatch
            |> Seq.filter (fun ts ->
                tripsToDelete |> Set.contains ts.tripId |> not)
            |> Seq.toArray
        calendar = Some calendar
        calendarExceptions = Some calendarExceptions
        feedInfo = None
    }
    feed
