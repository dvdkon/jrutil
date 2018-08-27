// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsMerge

open System.Collections.Generic
open System.Text.RegularExpressions

open JrUtil.GtfsModel

type IdMap = Map<string, string>

// Since this operation is performance-critical (merging tens of thousands
// of feeds isn't easy), it's implemented with mutable data structures.
type MergedFeed() =
    let agencies = new List<Agency>()
    // These index dictionaries are here for efficient lookup based on
    // a record's field. They map a value to a "pointer" into the
    // greater list.
    // Maybe I really should have written this in SQL...
    let agenciesById = new Dictionary<string, int>()
    let agenciesByName = new Dictionary<string, int>()
    let stops = new List<Stop>()
    let stopsById = new Dictionary<string, int>()
    let stopsByName = new Dictionary<string, HashSet<int>>()
    let stopRouteTypes = new Dictionary<int, string Set>()
    let routes = new List<Route>()
    let routesById = new Dictionary<string, int>()
    let routesByName = new Dictionary<string, int>()
    let trips = new List<Trip>()
    let tripsById = new Dictionary<string, int>()
    let stopTimes = new List<StopTime>()
    let calendar = new List<CalendarEntry>()
    let calendarEntryById = new Dictionary<string, int>()
    let calendarExceptions = new List<CalendarException>()
    let calendarExceptionsById = new Dictionary<string, HashSet<int>>()

    member this.GetNewId prevId (objs: Dictionary<string, _>) =
        if objs.ContainsKey(prevId) then
            Seq.initInfinite id |> Seq.pick (fun i ->
                let newId = sprintf "%s/%d" prevId i
                if objs.ContainsKey(newId)
                then None
                else Some newId
            )
        else prevId

    // "MultiDict" is kind of an "ad-hoc" name, so don't expect to see it
    // anywhere else.
    member this.AddToMultiDict (mdict: Dictionary<'a, HashSet<'b>>) key value =
        if mdict.ContainsKey(key) then
            mdict.[key].Add(value) |> ignore
        else
            let set = new HashSet<'b>()
            set.Add(value) |> ignore
            mdict.[key] <- set

    // The rest of this file is repetitive and boring code (and some comments).
    // It all follows the same pattern (Change ID to new ID, change foreign
    // IDs per ID maps).
    // TODO: Abstraction!

    member this.InsertAgency (agency: Agency) =
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
        if not <| agenciesByName.ContainsKey(agency.name) then
            let newId = this.GetNewId agencyId agenciesById
            let newAgency = {agency with id = Some newId}
            agencies.Add(newAgency)
            agenciesById.Add(newId, agencies.Count - 1)
            agenciesByName.Add(newAgency.name, agencies.Count - 1)
            newId
        else
            let existingAgencyIndex = agenciesByName.[agency.name]
            agencies.[existingAgencyIndex].id.Value

    member this.InsertAgencies agencies =
        agencies |> Array.map (fun a ->
            let newId = this.InsertAgency a
            (a.id |> Option.defaultValue "", newId))
        |> Map

    member this.StopRouteTypes (feed: GtfsFeed) (stop: Stop) =
        feed.stopTimes
        |> Set.ofArray
        |> Set.filter (fun st -> st.stopId = stop.id)
        |> Set.map (fun st ->
            let trip = feed.trips |> Array.find (fun t -> t.id = st.tripId)
            let route =
                feed.routes |> Array.find (fun r -> r.id = trip.routeId)
            route.routeType)

    member this.InsertStop feed (stationIdMap: IdMap) (stop: Stop) =
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

        let isRailwayStation =
            Set.exists (fun rt -> rt = "2" || Regex.IsMatch(rt, "1.."))

        let mergeCandidate =
            if stopsByName.ContainsKey(stop.name) then
                stopsByName.[stop.name]
                |> Seq.tryFind (fun si ->
                    let existingRouteTypes = stopRouteTypes.[si]
                    let newRouteTypes = this.StopRouteTypes feed stop
                    isRailwayStation existingRouteTypes =
                        isRailwayStation newRouteTypes
                )
                |> Option.map (fun si -> stops.[si])
            else None

        match mergeCandidate with
        | None ->
            let newId = this.GetNewId stop.id stopsById
            let newStop = {
                stop with
                    id = newId
                    parentStation =
                        stop.parentStation
                        |> Option.map (fun ps -> stationIdMap.[ps])
            }
            stops.Add(newStop)
            let index = stops.Count - 1
            stopsById.Add(newId, index)
            this.AddToMultiDict stopsByName newStop.name index
            stopRouteTypes.Add(index, this.StopRouteTypes feed newStop)
            newId
        | Some mc ->
            mc.id

    member this.InsertStops (feed: GtfsFeed) =
        let (stations, other) =
            feed.stops
            |> Array.partition (fun s -> s.locationType = Some Station)
        let stationIdMap =
            stations
            |> Array.map (fun s -> (s.id, this.InsertStop feed Map.empty s))
            |> Map
        let otherIdMap =
            other
            |> Array.map (fun s -> (s.id, this.InsertStop feed stationIdMap s))
        Map (Seq.append (Map.toSeq stationIdMap) otherIdMap)

    member this.InsertRoute (agencyIdMap: IdMap) (route: Route) =
        // For now this is done by comparing names, a better way might be
        // comparing a set of all stations
        let name =
            route.shortName |> Option.orElse route.longName |> Option.get
        let canMerge = routesByName.ContainsKey(name)

        if not canMerge then
            let newId = this.GetNewId route.id routesById
            let agencyId = route.agencyId |> Option.defaultValue ""
            let newRoute = {
                route with
                    id = newId
                    agencyId = Some agencyIdMap.[agencyId]
            }
            routes.Add(newRoute)
            let index = routes.Count - 1
            routesById.Add(newId, index)
            routesByName.Add(name, index)
            newId
        else
            let existingRouteIndex = routesByName.[name]
            routes.[existingRouteIndex].id

    member this.InsertRoutes agencyIdMap (routes: Route array) =
        routes
        |> Array.map (fun r -> (r.id, this.InsertRoute agencyIdMap r))
        |> Map

    member this.InsertCalendarEntry (calendarEntry: CalendarEntry) =
        let newId = this.GetNewId calendarEntry.id calendarEntryById
        let newCalEntry = {
            calendarEntry with
                id = newId
        }
        calendar.Add(newCalEntry)
        let index = calendar.Count - 1
        calendarEntryById.Add(newId, index)
        newId

    member this.InsertCalendar (calendarEntries: CalendarEntry array) =
        calendarEntries
        |> Array.map (fun ce -> (ce.id, this.InsertCalendarEntry ce))
        |> Map

    member this.InsertCalendarException
            (calendarIdMap: IdMap)
            (calendarException: CalendarException) =
        let newId =
            match calendarIdMap |> Map.tryFind calendarException.id with
            | Some id -> id
            | None -> this.GetNewId calendarException.id
                                    calendarExceptionsById
        let newCalExc = {
            calendarException with
                id = newId
        }
        calendarExceptions.Add(newCalExc)
        let index = calendarExceptions.Count - 1
        this.AddToMultiDict calendarExceptionsById newId index

        newId

    member this.InsertCalendarExceptions calendarIdMap calendarExceptions =
        calendarExceptions
        |> Array.fold
            (fun m ce ->
                let newId = this.InsertCalendarException m ce
                m |> Map.add ce.id newId)
            calendarIdMap

    member this.InsertTrip
            (routeIdMap: IdMap) (calendarIdMap: IdMap) (trip: Trip) =
        // TODO: Trips are never merged.
        let newId = this.GetNewId trip.id tripsById
        let newTrip = {
            trip with
                id = newId
                routeId = routeIdMap.[trip.routeId]
                serviceId = calendarIdMap.[trip.serviceId]
        }
        trips.Add(newTrip)
        let index = trips.Count - 1
        tripsById.Add(newId, index)
        newId

    member this.InsertTrips routeIdMap calendarIdMap (trips: Trip array) =
        trips
        |> Array.map
            (fun t -> (t.id, this.InsertTrip routeIdMap calendarIdMap t))
        |> Map

    member this.InsertStopTime
            (tripIdMap: IdMap) (stopIdMap: IdMap) (stopTime: StopTime) =
        let newStopTime = {
            stopTime with
                tripId = tripIdMap.[stopTime.tripId]
                stopId = stopIdMap.[stopTime.stopId]
        }
        stopTimes.Add(newStopTime)

    member this.InsertStopTimes tripIdMap stopIdMap stopTimes =
        stopTimes |> Array.iter (this.InsertStopTime tripIdMap stopIdMap)

    member this.InsertFeed (feed: GtfsFeed) =
        let agencyIdMap = this.InsertAgencies feed.agencies
        let stopIdMap = this.InsertStops feed
        let routeIdMap = this.InsertRoutes agencyIdMap feed.routes
        let calendarIdMap =
            this.InsertCalendar (feed.calendar |> Option.defaultValue [||])
        let calendarIdMap =
            this.InsertCalendarExceptions calendarIdMap
                                          (feed.calendarExceptions
                                           |> Option.defaultValue [||])
        let tripIdMap = this.InsertTrips routeIdMap calendarIdMap feed.trips
        this.InsertStopTimes tripIdMap stopIdMap feed.stopTimes

    member this.ToGtfsFeed () =
        let feed: GtfsFeed = {
            agencies = Array.ofSeq agencies
            stops = Array.ofSeq stops
            routes = Array.ofSeq routes
            trips = Array.ofSeq trips
            stopTimes = Array.ofSeq stopTimes
            calendar = Array.ofSeq calendar |> Some
            calendarExceptions = Array.ofSeq calendarExceptions |> Some
            feedInfo = None
        }
        feed
