// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfMerger

open System.Collections.Generic
open NodaTime
open Serilog
open NetTopologySuite.Geometries

open JrUtil.JdfModel
open JrUtil.Utils
open JrUtil.GeoData.Common

type JdfMerger() =
    let stops = ResizeArray()
    let stopPosts = ResizeArray()
    let agenciesByIco = MultiDict()
    let routesByLicNum = MultiDict()
    let routeIntegrationsByRoute = MultiDict()
    let routeStopsByRoute = MultiDict()
    let tripsByRoute = MultiDict()
    let tripGroups = ResizeArray()
    let tripStopsByRoute = MultiDict()
    let routeInfoByRoute = MultiDict()
    let attributeRefs = ResizeArray()
    let serviceNotesByRoute = MultiDict()
    let transfersByRoute = MultiDict()
    let agencyAlternationsByRoute = MultiDict()
    let alternateRouteNamesByRoute = MultiDict()
    let reservationOptionsByRoute = MultiDict()
    let stopLocationsByStop = Dictionary()

    let mutable lastStopId = 0
    let mutable lastAttributeRefId = 0
    let mutable lastTripGroupId = 0

    let attributeRefsByValue = Dictionary()
    let stopsByNames = Dictionary()
    let stopPostsSet = HashSet()
    let batchDateByRoute = Dictionary()

    let stopNameTuple (s: Stop) =
        s.town, s.district, s.nearbyPlace, s.regionId, s.country

    let locationDistance loc1 loc2 =
        let locToPt loc =
            wgs84Factory.CreatePoint(
                Coordinate(float loc.lon, float loc.lat))
            |> pointWgs84ToEtrs89Ex
        (locToPt loc1).Distance(locToPt loc2)
    let locationDistanceThresh = 1000

    member this.batch = {
        version = {
            version = "1.11"
            duNum = None
            region = None
            batchId = None
            creationDate = Some <| dateToday ()
            generator = Some "JrUtil JdfMerger"
        }
        stops = stops |> Seq.toArray
        stopPosts = stopPosts |> Seq.toArray
        agencies = agenciesByIco.Values |> Seq.collect id |> Seq.toArray
        routes = routesByLicNum.Values |> Seq.collect id |> Seq.toArray
        routeIntegrations =
            routeIntegrationsByRoute.Values |> Seq.collect id |> Seq.toArray
        routeStops =
            routeStopsByRoute.Values |> Seq.collect id |> Seq.toArray
        trips = tripsByRoute.Values |> Seq.collect id |> Seq.toArray
        tripGroups = tripGroups |> Seq.toArray
        tripStops = tripStopsByRoute.Values |> Seq.collect id |> Seq.toArray
        routeInfo = routeInfoByRoute.Values |> Seq.collect id |> Seq.toArray
        attributeRefs = attributeRefs |> Seq.toArray
        serviceNotes =
            serviceNotesByRoute.Values |> Seq.collect id |> Seq.toArray
        transfers = transfersByRoute.Values |> Seq.collect id |> Seq.toArray
        agencyAlternations =
            agencyAlternationsByRoute.Values |> Seq.collect id |> Seq.toArray
        alternateRouteNames =
            alternateRouteNamesByRoute.Values |> Seq.collect id |> Seq.toArray
        reservationOptions =
            reservationOptionsByRoute.Values |> Seq.collect id |> Seq.toArray
        stopLocations = stopLocationsByStop.Values |> Seq.toArray
    }

    member private this.deleteRoute(r: Route) =
        let routeId = r.id
        let routeDistinction = r.idDistinction
        routesByLicNum.[routeId].RemoveAll(fun r ->
            r.idDistinction = routeDistinction) |> ignore
        routeIntegrationsByRoute.Remove((routeId, routeDistinction)) |> ignore
        routeStopsByRoute.Remove((routeId, routeDistinction)) |> ignore
        tripsByRoute.Remove((routeId, routeDistinction)) |> ignore
        tripStopsByRoute.Remove((routeId, routeDistinction)) |> ignore
        routeInfoByRoute.Remove((routeId, routeDistinction)) |> ignore
        serviceNotesByRoute.Remove((routeId, routeDistinction)) |> ignore
        transfersByRoute.Remove((routeId, routeDistinction)) |> ignore
        agencyAlternationsByRoute.Remove((routeId, routeDistinction)) |> ignore
        alternateRouteNamesByRoute.Remove((routeId, routeDistinction)) |> ignore
        reservationOptionsByRoute.Remove((routeId, routeDistinction)) |> ignore
        batchDateByRoute.Remove((routeId, routeDistinction)) |> ignore

    member private this.copyRoute(copy: Route) =
        let oldDist = copy.idDistinction
        let newDist =
            (routesByLicNum.[copy.id]
             |> Seq.map (fun r -> r.idDistinction)
             |> Seq.max) + 1
        routesByLicNum.[copy.id].Add(
            {copy with idDistinction = newDist })
        routeIntegrationsByRoute.[(copy.id, newDist)] <-
            routeIntegrationsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        routeStopsByRoute.[(copy.id, newDist)] <-
            routeStopsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        tripsByRoute.[(copy.id, newDist)] <-
            tripsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        tripStopsByRoute.[(copy.id, newDist)] <-
            tripStopsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        routeInfoByRoute.[(copy.id, newDist)] <-
            routeInfoByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        serviceNotesByRoute.[(copy.id, newDist)] <-
            serviceNotesByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        transfersByRoute.[(copy.id, newDist)] <-
            transfersByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        agencyAlternationsByRoute.[(copy.id, newDist)] <-
            agencyAlternationsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        alternateRouteNamesByRoute.[(copy.id, newDist)] <-
            alternateRouteNamesByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        reservationOptionsByRoute.[(copy.id, newDist)] <-
            reservationOptionsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        batchDateByRoute.[(copy.id, newDist)] <-
            batchDateByRoute.[(copy.id, oldDist)]

    member private this.resolveOneRouteOverlap(r1, r2) =
        assert (r1.idDistinction <> r2.idDistinction)
        let r1Date = batchDateByRoute.[(r1.id, r1.idDistinction)]
        let r2Date = batchDateByRoute.[(r2.id, r2.idDistinction)]
        let r2Priority =
            (r2.detour && (not r1.detour || r1Date < r2Date))
            || (not r1.detour && r1Date < r2Date)
        let r1Priority = r1.detour && (not r2.detour || r2Date < r1Date)

        // Validity ranges don't overlap, keep both
        if r1.timetableValidTo < r2.timetableValidFrom
           || r1.timetableValidFrom > r2.timetableValidTo then ()
        // Ranges overlap exactly, keep only priority route
        else if r1.timetableValidFrom = r2.timetableValidFrom
             && r1.timetableValidTo = r2.timetableValidTo then
            let toDelete = if r2Priority then r1 else r2
            Log.Debug(
                "Route {LicNum} variants overlap exactly, removing {Dist}, \
                 keeping {Dist2}",
                r1.id, toDelete.idDistinction,
                (if r2Priority then r2 else r1).idDistinction)
            this.deleteRoute(toDelete)
        // Ranges start at the same day, cut one
        else if r1.timetableValidFrom = r2.timetableValidFrom
             && r1.timetableValidTo < r2.timetableValidTo then
            if r2Priority then
                Log.Debug("Route {LicNum} variants overlap with same start, \
                           removing {Dist}",
                          r1.id, r1.idDistinction)
                this.deleteRoute(r1)
            else
                Log.Debug("Route {LicNum} variants overlap with same start, \
                           cutting {Dist}",
                          r1.id, r2.idDistinction)
                routesByLicNum.[r2.id].Remove(r2) |> ignore
                routesByLicNum.[r2.id].Add(
                    {r2 with
                        timetableValidFrom =
                            r1.timetableValidTo + Period.FromDays(1)})
        // Ranges end at the same day, cut one
        else if r1.timetableValidTo = r2.timetableValidTo
             && r1.timetableValidFrom > r2.timetableValidFrom then
            if r2Priority then
                Log.Debug("Route {LicNum} variants overlap with same end, \
                           removing {Dist}",
                          r1.id, r1.idDistinction)
                this.deleteRoute(r1)
            else
                Log.Debug("Route {LicNum} variants overlap with same end, \
                           cutting {Dist}",
                          r1.id, r2.idDistinction)
                routesByLicNum.[r2.id].Remove(r2) |> ignore
                routesByLicNum.[r2.id].Add(
                    {r2 with
                        timetableValidTo =
                            r1.timetableValidFrom - Period.FromDays(1)})
        // r2 is a later variant, cut part of old route if not priority
        else if r1.timetableValidFrom < r2.timetableValidFrom
             && r1.timetableValidTo < r2.timetableValidTo then
            if not r1Priority then
                Log.Debug(
                    "Route {LicNum} variants overlap, cutting {Dist}",
                    r1.id, r1.idDistinction)
                routesByLicNum.[r1.id].Remove(r1) |> ignore
                routesByLicNum.[r1.id].Add(
                    {r1 with
                        timetableValidTo =
                            r2.timetableValidFrom - Period.FromDays(1)})
            else
                Log.Debug(
                    "Route {LicNum} variants overlap, cutting {Dist}, \
                     since {Dist2} has priority",
                    r1.id, r2.idDistinction, r1.idDistinction)
                routesByLicNum.[r2.id].Remove(r2) |> ignore
                routesByLicNum.[r2.id].Add(
                    {r2 with
                        timetableValidFrom =
                            r1.timetableValidTo + Period.FromDays(1)})
        // Validity ranges overlap and r2 is inside r1, duplicate r1
        // (if not priority)
        else if r1.timetableValidFrom < r2.timetableValidFrom
             && r1.timetableValidTo > r2.timetableValidTo then
            if r1Priority then
                Log.Debug(
                    "Route {LicNum} variants overlap and {Dist} is contained \
                     in {Dist2}, removing the former, since the latter has \
                     priority",
                    r1.id, r2.idDistinction, r1.idDistinction)
                this.deleteRoute(r2)
            else
                Log.Debug(
                    "Route {LicNum} variants overlap and {Dist} is contained \
                     in {Dist2}, splitting the latter",
                    r1.id, r2.idDistinction, r1.idDistinction)
                // Adjust r1's validity for first chunk of split
                routesByLicNum.[r1.id].Remove(r1) |> ignore
                routesByLicNum.[r1.id].Add(
                    {r1 with
                        timetableValidTo =
                            r2.timetableValidFrom - Period.FromDays(1)})
                // Duplicate of r1 for second chunk of split
                this.copyRoute(
                    {r1 with
                        timetableValidFrom =
                            r2.timetableValidTo + Period.FromDays(1)})
        // Resolve other overlaps by symmetry (simple overlap but r2 is first,
        // r1 is inside r2, same start/end date but r2 is shorter)
        else this.resolveOneRouteOverlap(r2, r1)

    member this.resolveRouteOverlaps() =
        for id in routesByLicNum.Keys do
            let handled = HashSet()
            // Pointer to mutable state
            let rsNow = routesByLicNum.[id]
            let unhandled () =
                rsNow
                |> Seq.tryFind (fun r ->
                    not <| handled.Contains(r.idDistinction))
            while unhandled () |> Option.isSome do
                let r2 = unhandled () |> Option.get
                handled.Add(r2.idDistinction) |> ignore
                let r1s =
                    rsNow
                    |> Seq.filter (fun r ->
                        r.idDistinction < r2.idDistinction)
                    |> Seq.sortBy (fun r -> r.idDistinction)
                    |> Seq.toArray
                for r1 in r1s do
                    // Take the newest version of r2
                    rsNow
                    |> Seq.tryFind (fun r ->
                        r.idDistinction = r2.idDistinction)
                    |> Option.iter (fun r2now ->
                        this.resolveOneRouteOverlap(r1, r2now))

    member this.add(batch: JdfBatch) =
        let existingAttributeRefs, attributeRefsToAdd =
            batch.attributeRefs
            |> splitSeq (fun ar ->
                attributeRefsByValue.ContainsKey(ar.value, ar.reserved1))
        let attributeRefsToAddNewId =
            attributeRefsToAdd
            |> Seq.map (fun ar ->
                lastAttributeRefId <- lastAttributeRefId + 1
                { ar with attributeId = lastAttributeRefId })
            |> Seq.cache
        attributeRefs.AddRange(attributeRefsToAddNewId)
        for ar in attributeRefsToAddNewId do
            attributeRefsByValue.[(ar.value, ar.reserved1)] <- ar.attributeId
        let attributeRefIdMap =
            Seq.concat [
                Seq.zip attributeRefsToAdd attributeRefsToAddNewId
                |> Seq.map (fun (ar, arni) ->
                    ar.attributeId, arni.attributeId)

                existingAttributeRefs
                |> Seq.map (fun ar ->
                    ar.attributeId, attributeRefsByValue.[(ar.value, ar.reserved1)])
            ]
            |> Map
        let mapAttributes =
            Array.map (Option.map (fun id -> attributeRefIdMap.[id]))

        let existingStops, stopsToAdd =
            batch.stops
            |> splitSeq (fun s ->
                stopsByNames.ContainsKey(stopNameTuple s))
        let stopsToAddIds = stopsToAdd |> Seq.map (fun s -> s.id) |> Set
        let stopsToAddNewIds =
            stopsToAdd
            |> Seq.map (fun s ->
                lastStopId <- lastStopId + 1
                { s with
                    id = lastStopId
                    attributes = mapAttributes s.attributes }
            )
            |> Seq.cache
        stops.AddRange(stopsToAddNewIds)
        for s in stopsToAddNewIds do
            stopsByNames.[stopNameTuple s] <- s.id
        let stopIdMap =
            Seq.concat [
                Seq.zip stopsToAdd stopsToAddNewIds
                |> Seq.map (fun (s, sni) -> s.id, sni.id)

                existingStops
                |> Seq.map (fun s -> s.id, stopsByNames.[stopNameTuple s])
            ]
            |> Map
        for sl in batch.stopLocations do
            let stopId = stopIdMap.[sl.stopId]
            let hasOldSl, oldSl = stopLocationsByStop.TryGetValue(stopId)

            // Check if old location isn't to far
            if hasOldSl
               && oldSl.precision = StopPrecise
               && sl.precision = StopPrecise
               && (oldSl.lat <> sl.lat || oldSl.lon <> sl.lon)
               && locationDistance sl oldSl > locationDistanceThresh then
                Log.Warning("Stop location for {StopId} in new batch is too \
                             far from existing location: {Lat}, {Lon}",
                            stopId, sl.lat, sl.lon)

            // Either this is a new location or an upgrade (town precise to
            // stop precise)
            if stopsToAddIds |> Set.contains sl.stopId
               || not <| hasOldSl
               || (sl.precision = StopPrecise
                   && oldSl.precision = TownPrecise)
            then
                stopLocationsByStop.[stopId] <- { sl with stopId = stopId }

        let stopPostsToAdd =
            stopPosts
            |> Seq.filter (fun sp ->
                not <| stopPostsSet.Contains((stopIdMap.[sp.stopId], sp.stopPostId)))
            |> Seq.map (fun sp -> { sp with stopId = stopIdMap.[sp.stopId] })
        stopPosts.AddRange(stopPostsToAdd)
        for sp in stopPostsToAdd do
            stopPostsSet.Add((sp.stopId, sp.stopPostId)) |> ignore

        let existingAgenciesMap =
            batch.agencies
            |> Seq.map (fun a ->
                a,
                agenciesByIco.[a.id]
                |> Seq.tryFind (fun a2 ->
                    a = {a2 with idDistinction = a.idDistinction}))
            |> Seq.cache
        let agenciesToAdd =
            existingAgenciesMap
            |> Seq.filter (fun (_, e) -> Option.isNone e)
            |> Seq.map fst
            |> Seq.cache
        let agenciesToAddNewId =
            agenciesToAdd
            |> Seq.map (fun a ->
                let other: Agency ResizeArray = agenciesByIco.[a.id]
                let subId =
                    if other |> Seq.isEmpty then 1
                    else (other
                          |> Seq.map (fun a -> a.idDistinction)
                          |> Seq.max) + 1
                let ani = { a with idDistinction = subId }
                // We need to add it right away, in case the input has multiple
                // agencies of the same ID
                agenciesByIco.[a.id].Add(ani)
                ani)
            |> Seq.toArray
        let agencyIdMap =
            Seq.concat [
                Seq.zip agenciesToAdd agenciesToAddNewId
                |> Seq.map (fun (a, ani) ->
                    (a.id, a.idDistinction), (ani.id, ani.idDistinction))

                existingAgenciesMap
                |> Seq.choose (fun (a, eo) ->
                    eo |> Option.map (fun e ->
                        (a.id, a.idDistinction), (e.id, e.idDistinction)))
            ]
            |> Map

        // We don't resolve validity overlaps here and leave that for a
        // post-processing phase
        let newRoutes =
            batch.routes
            |> Array.map (fun r ->
                let other = routesByLicNum.[r.id]
                let aid, aidd = agencyIdMap.[(r.agencyId, r.agencyDistinction)]
                {r with
                    idDistinction = (Seq.length other) + 1
                    agencyId = aid
                    agencyDistinction = aidd})
        for r in newRoutes do
            routesByLicNum.[r.id].Add(r)

            batchDateByRoute.[(r.id, r.idDistinction)] <-
                batch.version.creationDate
        let routeIdMap =
            Seq.zip batch.routes newRoutes
            |> Seq.map (fun (r, rni) ->
                (r.id, r.idDistinction), (rni.id, rni.idDistinction))
            |> Map

        batch.routeIntegrations
        |> Seq.map (fun ri ->
            let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
            { ri with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeIntegrationsByRoute.[k] <- v)

        batch.routeStops
        |> Seq.map (fun rs ->
            let rid, ridd = routeIdMap.[(rs.routeId, rs.routeDistinction)]
            { rs with
                routeId = rid
                routeDistinction = ridd
                stopId = stopIdMap.[rs.stopId]
                attributes = mapAttributes rs.attributes
            })
        |> Seq.groupBy (fun rs -> rs.routeId, rs.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeStopsByRoute.[k] <- v)

        let newTripGroups =
            batch.tripGroups
            |> Seq.map (fun tg ->
                lastTripGroupId <- lastTripGroupId + 1
                { tg with
                    id = lastTripGroupId
                })
        tripGroups.AddRange(newTripGroups)
        let tripGroupIdMap =
            Seq.zip batch.tripGroups newTripGroups
            |> Seq.map (fun (tg, tgni) -> tg.id, tgni.id)
            |> Map

        batch.trips
        |> Seq.map (fun t ->
            let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
            { t with
                routeId = rid
                routeDistinction = ridd
                tripGroupId =
                    t.tripGroupId
                    |> Option.map (fun id -> tripGroupIdMap.[id])
                attributes = mapAttributes t.attributes
            })
        |> Seq.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Seq.iter (fun (k, v) -> tripsByRoute.[k] <- v)

        batch.tripStops
        |> Seq.map (fun ts ->
            let rid, ridd = routeIdMap.[(ts.routeId, ts.routeDistinction)]
            { ts with
                routeId = rid
                routeDistinction = ridd
                stopId = stopIdMap.[ts.stopId]
                attributes = mapAttributes ts.attributes
            })
        |> Seq.groupBy (fun ts -> ts.routeId, ts.routeDistinction)
        |> Seq.iter (fun (k, v) -> tripStopsByRoute.[k] <- v)

        batch.routeInfo
        |> Seq.map (fun ri ->
            let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
            { ri with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeInfoByRoute.[k] <- v)

        batch.serviceNotes
        |> Seq.map (fun sn ->
            let rid, ridd = routeIdMap.[(sn.routeId, sn.routeDistinction)]
            { sn with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun sn -> sn.routeId, sn.routeDistinction)
        |> Seq.iter (fun (k, v) -> serviceNotesByRoute.[k] <- v)

        batch.transfers
        |> Seq.map (fun t ->
            let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
            { t with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Seq.iter (fun (k, v) -> transfersByRoute.[k] <- v)

        batch.agencyAlternations
        |> Seq.map (fun aa ->
            let rid, ridd = routeIdMap.[(aa.routeId, aa.routeDistinction)]
            let aid, aidd = agencyIdMap.[(aa.agencyId, aa.agencyDistinction)]
            { aa with
                routeId = rid
                routeDistinction = ridd
                agencyId = aid
                agencyDistinction = aidd
                attributes = mapAttributes aa.attributes
            })
        |> Seq.groupBy (fun aa -> aa.routeId, aa.routeDistinction)
        |> Seq.iter (fun (k, v) -> agencyAlternationsByRoute.[k] <- v)

        batch.alternateRouteNames
        |> Seq.map (fun arn ->
            let rid, ridd = routeIdMap.[(arn.routeId, arn.routeDistinction)]
            { arn with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun arn -> arn.routeId, arn.routeDistinction)
        |> Seq.iter (fun (k, v) -> alternateRouteNamesByRoute.[k] <- v)

        batch.reservationOptions
        |> Seq.map (fun ro ->
            let rid, ridd = routeIdMap.[(ro.routeId, ro.routeDistinction)]
            { ro with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ro -> ro.routeId, ro.routeDistinction)
        |> Seq.iter (fun (k, v) -> reservationOptionsByRoute.[k] <- v)

        ()
