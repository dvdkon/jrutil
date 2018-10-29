// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsMerge

open System.Data.Common

open JrUtil.GtfsModel
open JrUtil.SqlRecordStore
open System.Text.RegularExpressions


type IdMap = Map<string, string>

type MergedFeed(conn: DbConnection, ?checkStopType: bool) =
    let checkStopType = defaultArg checkStopType true

    let mutable feedNum = 0

    let newId oldId =
        sprintf "%d/%s" feedNum oldId

    let insertAgency (agency: Agency) =
        // If this agency is already in the merged feed (determined by
        // comparing the name), it's not inserted and the existing agency's id
        // is returned. The two agency objects aren't merged in any way
        // (except for IDs). If a need for field-level merging arises, it will
        // probably be added only for the fields that will need it, or possibly
        // record-wide with reflection magic.
        // This also applies to the other parts of the feed.
        let agencyId = agency.id |> Option.defaultValue ""
        // Matching by name equality like this is not ideal.
        // Some sort of fuzzy matching would probably be best here.
        let existingId =
            sqlQueryOne conn "SELECT id FROM agencies WHERE name = @n"
                        ["n", box agency.name]
            |> unbox
        if existingId <> null then
            let newId = newId agencyId
            let newAgency = {agency with id = Some newId}
            sqlInsert conn "agencies" newAgency
            newId
        else
            existingId

    let insertAgencies (agencies: Agency seq) =
        agencies
        |> Seq.map (fun a -> (a.id |> Option.defaultValue "", insertAgency a))
        |> Map

    let stopRouteTypes (feed: GtfsFeed) (stop: Stop) =
        feed.stopTimes
        |> Set.ofArray
        |> Set.filter (fun st -> st.stopId = stop.id)
        |> Set.map (fun st ->
            let trip = feed.trips |> Array.find (fun t -> t.id = st.tripId)
            let route =
                feed.routes |> Array.find (fun r -> r.id = trip.routeId)
            route.routeType)

    let insertStop feed (stationIdMap: IdMap) (stop: Stop) =
        // The best we could do here is probably fuzzy matching while also
        // looking at adjacent stops via trip stop lists.
        // Such an approach might benefit from whole-dataset analysis
        // but that would make this whole process much more complex and
        // also much slower.
        // TODO: This won't work, because there are stops with identical
        // names that are not on the same spot (for example, Hostivice bus stop
        // and Hostivice train station). Probably the only way to do this
        // "properly" is to find the stations' coordinates beforehand and then
        // merge.
        // Another problem is directional stops (two stops on two sides
        // of a road).
        // Separating railway stations and every other stop should give
        // "good enough" results, probably the best possible without
        // coordinates.

        // TODO: PG fulltext search
        // TODO: Indexed function?
        let mergeId =
            if checkStopType then
                let isRailwayStation =
                    stopRouteTypes feed stop
                    |> Set.exists
                        (fun rt -> rt = "2" || Regex.IsMatch(rt, "1.."))

                let sql =
                    """
                    SELECT id FROM stops AS s
                    WHERE name = @n
                        AND (EXISTS (
                            SELECT * FROM "stopTimes" AS st
                                INNER JOIN trips AS t ON (t.id = st."tripId")
                                INNER JOIN routes AS r ON (r.id = t."routeId")
                            WHERE st."stopId" = s.id
                                AND (r."routeType" = '2'
                                    OR r."routeType" LIKE '1__')
                        )) = @rs
                    """
                sqlQuery conn sql
                    ["n", box stop.name; "rs", box isRailwayStation]
            else
                let sql = "SELECT id FROM stops WHERE name = @n"
                sqlQuery conn sql
                    ["n", box stop.name]


        match mergeId |> Seq.toList with
        | [] ->
            let newId = newId stop.id
            let newStop = {
                stop with
                    id = newId
                    parentStation =
                        stop.parentStation
                        |> Option.map (fun ps -> stationIdMap.[ps])
            }
            sqlInsert conn "stops" newStop
            newId
        | [row] -> unbox row.[0]
        | _ -> failwithf "More than one merge candidate for stop \"%s\""
                         stop.name

    let insertStops (feed: GtfsFeed) =
        // Stations and other stops have to be processed separately, because
        // stops may reference stations and so we need the ID map
        let (stations, other) =
            feed.stops
            |> Array.partition (fun s -> s.locationType = Some Station)
        let stationIdMap =
            stations
            |> Array.map (fun s -> (s.id, insertStop feed Map.empty s))
            |> Map
        let otherIdMap =
            other
            |> Array.map (fun s -> (s.id, insertStop feed stationIdMap s))
        Map (Seq.append (Map.toSeq stationIdMap) otherIdMap)

    let insertRoute (agencyIdMap: IdMap) (route: Route) =
        // For now this is done by comparing names, a better way might be
        // comparing a set of all stations
        let name =
            route.shortName |> Option.orElse route.longName |> Option.get
        let mergeId =
            sqlQuery conn """
            SELECT id FROM routes
            WHERE COALESCE("shortName", "longName") = @n
            """ ["n", box name]

        match mergeId |> Seq.toList with
        | [] ->
            let agencyId = route.agencyId |> Option.defaultValue ""
            let nid = newId route.id
            let newRoute = {
                route with
                    id = nid
                    agencyId = Some agencyIdMap.[agencyId]
            }
            sqlInsert conn "routes" newRoute
            nid
        | [row] -> unbox row.[0]
        | _ -> failwithf "More than one merge candidate for route \"%s\"" name

    let insertRoutes agencyIdMap (routes: Route array) =
        routes |> Array.map (fun r -> (r.id, insertRoute agencyIdMap r)) |> Map

    let insertCalendarEntry (calendarEntry: CalendarEntry) =
        let nid = newId calendarEntry.id
        let newCalEntry = {
            calendarEntry with
                id = nid
        }
        sqlInsert conn "calendar" newCalEntry
        nid

    let insertCalendar (calendar: CalendarEntry array) =
        calendar
        |> Array.map (fun ce -> (ce.id, insertCalendarEntry ce))
        |> Map

    let insertCalendarException (calendarIdMap: IdMap)
                                (calExc: CalendarException) =
        let nid =
            calendarIdMap
            |> Map.tryFind calExc.id
            |> Option.defaultValue (newId calExc.id)
        let newCalExc = {
            calExc with
                id = nid
        }
        sqlInsert conn "calendarExceptions" newCalExc
        nid

    let insertCalendarExceptions calendarIdMap calExcs =
        calExcs
        |> Array.fold
            (fun m ce ->
                let newId = insertCalendarException m ce
                m |> Map.add ce.id newId)
            calendarIdMap

    let insertTrip (routeIdMap: IdMap) (calendarIdMap: IdMap) (trip: Trip) =
        // TODO: Trips are never merged
        let nid = newId trip.id
        let newTrip = {
            trip with
                id = nid
                routeId = routeIdMap.[trip.routeId]
                serviceId = calendarIdMap.[trip.serviceId]
        }
        sqlInsert conn "trips" newTrip
        nid

    let insertTrips routeIdMap calendarIdMap (trips: Trip array) =
        trips
        |> Array.map (fun t -> (t.id, insertTrip routeIdMap calendarIdMap t))
        |> Map

    let insertStopTime
            (tripIdMap: IdMap) (stopIdMap: IdMap) (stopTime: StopTime) =
        let newStopTime = {
            stopTime with
                tripId = tripIdMap.[stopTime.tripId]
                stopId = stopIdMap.[stopTime.stopId]
        }
        sqlInsert conn "stopTimes" newStopTime

    let insertStopTimes tripIdMap stopIdMap stopTimes =
        stopTimes |> Array.iter (insertStopTime tripIdMap stopIdMap)

    member this.InsertFeed feed =
        feedNum <- feedNum + 1
        let agencyIdMap = insertAgencies feed.agencies
        let stopIdMap = insertStops feed
        let routeIdMap = insertRoutes agencyIdMap feed.routes
        let calendarIdMap =
            insertCalendar (feed.calendar |> Option.defaultValue [||])
        let calendarIdMap =
            insertCalendarExceptions
                calendarIdMap
                (feed.calendarExceptions |> Option.defaultValue [||])
        let tripIdMap = insertTrips routeIdMap calendarIdMap feed.trips
        insertStopTimes tripIdMap stopIdMap feed.stopTimes

    member this.CreateTables() =
        let table recType name pkeyCols indexCols =
            createTableFor conn recType name
            let pkey =
                pkeyCols
                |> Seq.map (fun f -> sprintf "\"%s\"" f)
                |> String.concat ", "
            executeSql conn
                       (sprintf """ALTER TABLE "%s" ADD PRIMARY KEY (%s)"""
                                name pkey) []
            for ic in indexCols do
                executeSql conn (sprintf """CREATE INDEX ON "%s" ("%s")"""
                                         name ic) []

        table typeof<Agency> "agencies" ["id"] ["name"]
        table typeof<Stop> "stops" ["id"] ["name"]
        table typeof<Route> "routes" ["id"]
            ["agencyId"; "shortName"; "longName"]
        table typeof<Trip> "trips" ["id"] ["routeId"; "serviceId"]
        table typeof<StopTime> "stopTimes" ["tripId"; "stopSequence"] []
        table typeof<CalendarEntry> "calendar" ["id"] []
        table typeof<CalendarException> "calendarExceptions" ["id"; "date"] []

    member this.ToGtfsFeed() = {
        feedInfo = None
        agencies = sqlQueryRec conn "SELECT * FROM agencies" [] |> Array.ofSeq
        stops = sqlQueryRec conn "SELECT * FROM stops" [] |> Array.ofSeq
        routes = sqlQueryRec conn "SELECT * FROM routes" [] |> Array.ofSeq
        trips = sqlQueryRec conn "SELECT * FROM trips" [] |> Array.ofSeq
        stopTimes =
            sqlQueryRec conn """SELECT * FROM "stopTimes" """ [] |> Array.ofSeq
        calendar =
            sqlQueryRec conn "SELECT * FROM calendar" [] |> Array.ofSeq |> Some
        calendarExceptions =
            sqlQueryRec conn """SELECT * FROM "calendarExceptions" """ []
            |> Array.ofSeq
            |> Some
    }
