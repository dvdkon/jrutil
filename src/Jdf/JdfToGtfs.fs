// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.JdfToGtfs

open JrUtil
open System
open System.Text.RegularExpressions

// Information that is lost in the conversion: TBD

let jdfAgencyId id idDistinction =
    sprintf "JDFA-%d-%d" id idDistinction

let jdfStopId id =
    sprintf "JDFS-%d" id

let jdfRouteId id idDistinction =
    sprintf "JDFR-%s-%d" id idDistinction

let jdfTripId routeId routeDistinction id =
    sprintf "JDFT-%s-%d-%d" routeId routeDistinction id

let getGtfsRouteType (jdfRoute: JdfModel.Route) =
    match (jdfRoute.transportMode, jdfRoute.routeType) with
    | (JdfModel.Bus, JdfModel.City)
    | (JdfModel.Bus, JdfModel.CityAndAdjacent) -> "704" // Local bus
    | (JdfModel.Bus, JdfModel.InternationalNoNational) // International coach
    | (JdfModel.Bus, JdfModel.InternationalOrNational) -> "201"
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
    // TODO: A prettier way of doing this
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
        url = jdfAgency.website |> Option.defaultValue ""
        // This will have to be adjusted for slovak datasets
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = Some jdfAgency.officePhoneNum
        fareUrl = None
        email = jdfAgency.email |> Option.defaultValue ""
    }

let getGtfsStops: JdfModel.JdfBatch -> GtfsModel.Stop array = fun jdfBatch ->
    // This doesn't deal with JDF StopPosts yet, but it'll likely use
    // the "Station platforms" extension
    jdfBatch.stops
    |> Array.map (fun jdfStop ->
    {
        // Here the mapping from JDF to GTFS starts becoming non-trivial
        // For example, JDF doesn't contain the positions of stops, so
        // this function will give them as 0,0 and rely on other sources to
        // fill them later.
        // Another problem is "zoneId", which is given per-stop in GTFS,
        // but per-stop-of-route in JDF. This function assumes that
        // the zone of a stop doesn't change, which may or may not be true.
        id = jdfStopId jdfStop.id
        code = None
        name = getStopName jdfStop
        desc = None
        lat = 0m
        lon = 0m
        // How does this even work with stops that are in multiple
        // transport systems and therefore zones?
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

        // TODO: Think of the best way to convey other JDF attributes
        // A column of 0/1 for each or a "set of strings" column?
    })

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
        desc = None
        // TODO: Deal with routes that don't allow national service
        // (only international)
        routeType = getGtfsRouteType jdfRoute
        url = None
        color = None
        textColor = None
        sortOrder = None
    })

let getGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routes
    |> Array.collect (fun jdfRoute ->
        let fromDate = jdfRoute.timetableValidFrom
        let toDate = jdfRoute.timetableValidTo
        jdfBatch.trips
        |> Array.filter
            (fun t -> t.routeId = jdfRoute.id
                      && t.routeDistinction = jdfRoute.idDistinction)
        |> Array.map (fun jdfTrip ->
            let attrs = Jdf.parseAttributes  jdfBatch jdfTrip.attributes
            let servicedDays =
                attrs
                |> Set.toList
                |> List.collect (fun a ->
                    match a with
                    // TODO: Account for state holidays
                    | JdfModel.WeekdayService -> [1; 2; 3; 4; 5]
                    | JdfModel.HolidaySundayService -> [7]
                    | JdfModel.DayOfWeekService(d) -> [d]
                    | _ -> []
                )

            let jdfRouteTimes =
                jdfBatch.routeTimes
                |> Array.filter (fun rt ->
                    rt.routeId = jdfRoute.id
                    && rt.routeDistinction = jdfRoute.idDistinction
                    && rt.tripId = jdfTrip.id)

            let serviceOnlyEntryExists =
                jdfRouteTimes
                |> Array.exists
                    (fun rt -> rt.timeType = Some JdfModel.ServiceOnly)
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


let rec dateRange (startDate: DateTime) (endDate: DateTime)  =
    // Create a list of DateTime objects containing all days between
    // startDate and endDate *inclusive* (TODO: Is this the right
    // thing to do with respect to JDF?)
    if startDate <= endDate
    then startDate :: (dateRange (startDate.AddDays(1.0)) endDate)
    else []

let getGtfsCalendarExceptions:
        JdfModel.JdfBatch -> GtfsModel.CalendarException array =
    let jdfDateRange (startDate: DateTime option) endDate =
        assert startDate.IsSome
        let endDate =
            match endDate with
            | Some ed -> ed
            | None -> startDate.Value
        dateRange startDate.Value endDate

    let designationNumRegex = new Regex(@"\d\d")
    fun jdfBatch ->
        jdfBatch.routeTimes
        |> Array.mapi (fun i jdfRouteTime ->
            if designationNumRegex.IsMatch(jdfRouteTime.designation) then
                let id = jdfTripId jdfRouteTime.routeId
                                   jdfRouteTime.routeDistinction
                                   jdfRouteTime.tripId
                let jdfCalExc date excType =
                    let exc: GtfsModel.CalendarException = {
                        id = id
                        date = date
                        exceptionType = excType
                    }
                    exc

                // None would indicate data that this function doesn't know
                // how to deal with
                let timeType = jdfRouteTime.timeType.Value
                match timeType with
                | JdfModel.Service ->
                    jdfDateRange jdfRouteTime.dateFrom jdfRouteTime.dateTo
                    |> List.map
                        (fun date -> jdfCalExc date GtfsModel.ServiceAdded)
                | JdfModel.ServiceAlso | JdfModel.ServiceOnly ->
                    [jdfCalExc jdfRouteTime.dateFrom.Value
                               GtfsModel.ServiceAdded]
                | JdfModel.NoService ->
                    [jdfCalExc jdfRouteTime.dateFrom.Value
                               GtfsModel.ServiceRemoved]
                | _ -> [] //TBD
                // Service only on odd/even weeks will require also looking
                // at the trip's attributes to determine which weekdays to
                // include
            else [] // Should be dealt with in another function
        )
        |> List.concat
        |> List.toArray


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
            directionId = None // TODO
            blockId = None
            shapeId = None
            wheelchairAccessible =
                Some (if wheelchairAccessible then "1" else "0")
            bikesAllowed =
                Some (if attrs |> Set.contains JdfModel.BicycleTransport
                      then GtfsModel.OneOrMore
                      else GtfsModel.None)
        })
    trips

let getGtfsStopTimes (jdfBatch: JdfModel.JdfBatch) =
    let dateTimeToTimeSpan (dtTime: DateTime) =
        new TimeSpan(dtTime.Hour, dtTime.Minute, dtTime.Second)

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
        // Multiple stops can have the same kilometer number, so simply
        // sorting them by kilometer to get them in "time order"
        // isn't enough
        // They are (hopefully) sorted in "RouteStop order" in the JDF
        // data, though, so what we actually have to do is determine
        // if this trip is "backwards" and reverse the whole TripStop list
        // if that's the case
        // This might break when a trip goes through the stops differently
        // than in RouteStop order and also crosses midnight,
        // but that's very unlikely and it wouldn't be a fault of this
        // code, but rather the JDF format, which can't represent
        // such trips.
        assert (jdfTripStops.Length >= 2)
        let firstKm = (jdfTripStops |> Array.head).kilometer
        let lastKm = (jdfTripStops |> Array.last).kilometer
        let jdfTripStops =
            (if firstKm > lastKm
             then Array.rev
             else id) jdfTripStops

        let mutable dayTimeSpan = new TimeSpan()

        jdfTripStops
        |> Array.mapi (fun i jdfTripStop ->
            // TODO: This will deal with the majority of trips, but some
            // might fail. I'll deal with them when I get data samples.
            match jdfTripStop.departureTime with
            | Some JdfModel.Passing | Some JdfModel.NotPassing -> None
            | _ ->
                let tripStopTimeExtract tst =
                    tst
                    |> Option.map (fun x ->
                        match x with
                        | JdfModel.Time dt -> dt
                        | _ -> failwith "Invalid data"
                    )

                let depTimeDT = tripStopTimeExtract jdfTripStop.departureTime
                let arrTimeDT = tripStopTimeExtract jdfTripStop.arrivalTime

                jdfTripStops
                |> Array.take i
                |> Array.tryPick (fun ts ->
                    match ts.departureTime with
                    | Some (JdfModel.Time depTime) -> Some depTime
                    | _ -> None
                )
                |> Option.iter (fun prevTime ->
                    if match (arrTimeDT, depTimeDT) with
                       | (Some dt, _) -> prevTime > dt
                       | (_, Some dt) -> prevTime > dt
                       | _ -> false
                    then dayTimeSpan <- dayTimeSpan + (new TimeSpan(24, 0, 0))
                )

                let depTime =
                    depTimeDT
                    |> Option.map
                        (fun dt -> dateTimeToTimeSpan dt + dayTimeSpan)

                let arrTime =
                    arrTimeDT
                    |> Option.map
                        (fun dt -> dateTimeToTimeSpan dt + dayTimeSpan)

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
                    stopId = jdfStopId jdfTripStop.stopId
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
                }
                Some stopTime
            )
        |> Array.choose id
    )

let getGtfsFeed (jdfBatch: JdfModel.JdfBatch) =
    let feed: GtfsModel.GtfsFeed = {
        agencies = jdfBatch.agencies |> Array.map convertToGtfsAgency
        stops = getGtfsStops jdfBatch
        routes = getGtfsRoutes jdfBatch
        trips = getGtfsTrips jdfBatch
        stopTimes = getGtfsStopTimes jdfBatch
        calendar = getGtfsCalendar jdfBatch
        calendarExceptions = getGtfsCalendarExceptions jdfBatch
        feedInfo = None
    }
    feed
