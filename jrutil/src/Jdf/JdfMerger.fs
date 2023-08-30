// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfMerger

open System.Collections.Generic
open NodaTime

open JrUtil.JdfModel
open JrUtil.Utils

type JdfMerger() =
    let stops = ResizeArray()
    let stopPosts = ResizeArray()
    let agenciesByName = Dictionary()
    let routesByLicNum = Dictionary<string, Route ResizeArray>()
    let routeIntegrations = ResizeArray()
    let routeStops = ResizeArray()
    let trips = ResizeArray()
    let tripGroups = ResizeArray()
    let tripStops = ResizeArray()
    let routeInfo = ResizeArray()
    let attributeRefs = ResizeArray()
    let serviceNotes = ResizeArray()
    let transfers = ResizeArray()
    let agencyAlternations = ResizeArray()
    let alternateRouteNames = ResizeArray()
    let reservationOptions = ResizeArray()
    let stopLocations = ResizeArray()

    let mutable lastStopId = 0
    let mutable lastAttributeRefId = 0
    let mutable lastTripGroupId = 0
    
    let attributeRefsByValue = Dictionary()
    let stopsByNames = Dictionary()
    let stopPostsSet = HashSet()
    let agenciesByIco = Dictionary()
    
    let stopNameTuple (s: Stop) =
        s.town, s.district, s.nearbyPlace, s.regionId, s.country

    member this.Batch = {
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
        agencies = agenciesByName.Values |> Seq.toArray
        routes = routesByLicNum.Values |> Seq.collect id |> Seq.toArray
        routeIntegrations = routeIntegrations |> Seq.toArray
        routeStops = routeStops |> Seq.toArray
        trips = trips |> Seq.toArray
        tripGroups = tripGroups |> Seq.toArray
        tripStops = tripStops |> Seq.toArray
        routeInfo = routeInfo |> Seq.toArray
        attributeRefs = attributeRefs |> Seq.toArray
        serviceNotes = serviceNotes |> Seq.toArray
        transfers = transfers |> Seq.toArray
        agencyAlternations = agencyAlternations |> Seq.toArray
        alternateRouteNames = alternateRouteNames |> Seq.toArray
        reservationOptions = reservationOptions |> Seq.toArray
        stopLocations = stopLocations |> Seq.toArray
    }


    member private this.resolveRouteOverlaps other r =
        let mutable r = r
        let mutable nextSubId = other |> List.length
        let resolvedOther = 
            other
            |> List.collect (fun otr ->
                // Validity ranges overlap exactly, keep only the new route
                if otr.timetableValidFrom = r.timetableValidFrom
                   && otr.timetableValidTo = r.timetableValidTo then
                    []
                // Validity ranges don't overlap, keep both
                else if otr.timetableValidTo < r.timetableValidFrom
                     || otr.timetableValidFrom > r.timetableValidTo then
                    [otr]
                // Validity ranges overlap and the new route is a later variant,
                // cut part of old route
                else if otr.timetableValidFrom < r.timetableValidFrom
                     && otr.timetableValidTo <= r.timetableValidTo then
                    [{otr with
                        timetableValidTo =
                            r.timetableValidFrom - Period.FromDays(1)}]
                // Validity ranges overlap and the current route is inside the old
                // route, cut old route in two
                // TODO: What if the new route contains the old one?
                else if otr.timetableValidFrom < r.timetableValidFrom
                     && otr.timetableValidTo > r.timetableValidTo then
                    let secondRoute = 
                        {otr with
                            timetableValidFrom =
                                r.timetableValidTo + Period.FromDays(1) }
                    // We have to copy in all the second route's data 
                    [
                        {otr with
                            timetableValidTo =
                                r.timetableValidFrom - Period.FromDays(1) }
                        secondRoute
                    ]
                // Validity ranges overlap and the new route is a
                // previous variant, cut part of new route
                else
                    r <- { r with
                             timetableValidFrom =
                                 otr.timetableValidTo
                                 + Period.FromDays(1) }
                    [otr])
        List.append resolvedOther [
            {r with idDistinction = nextSubId + 1}
        ]
    
    member this.Add(batch: JdfBatch) =
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
                    attributes =
                        s.attributes |> Array.map (Option.map (fun id ->
                            attributeRefIdMap.[id]))
                    }
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
        stopLocations.AddRange(
            batch.stopLocations
            |> Seq.filter (fun sl -> stopsToAddIds |> Set.contains sl.stopId)
            |> Seq.map (fun sl ->
                {sl with stopId = stopIdMap.[sl.stopId] })
        )

        let stopPostsToAdd =
            stopPosts
            |> Seq.filter (fun sp ->
                not <| stopPostsSet.Contains((stopIdMap.[sp.stopId], sp.stopPostId)))
            |> Seq.map (fun sp -> { sp with stopId = stopIdMap.[sp.stopId] })
        stopPosts.AddRange(stopPostsToAdd)
        for sp in stopPostsToAdd do
            stopPostsSet.Add((sp.stopId, sp.stopPostId)) |> ignore
            
        let existingAgencies, agenciesToAdd =
            batch.agencies
            |> splitSeq (fun a -> agenciesByName.ContainsKey(a.name))
        let agenciesToAddNewId =
            agenciesToAdd
            |> Seq.map (fun a ->
                let notFirst, (other: Agency ResizeArray) =
                    agenciesByIco.TryGetValue(a.id)
                let subId =
                    if notFirst
                    then (other
                          |> Seq.map (fun a -> a.idDistinction)
                          |> Seq.max) + 1
                    else 1
                { a with idDistinction = subId })
            |> Seq.cache
        for a in agenciesToAddNewId do
            agenciesByName.[a.name] <- a

            if not <| agenciesByIco.ContainsKey(a.id) then
                agenciesByIco.[a.id] <- ResizeArray()
            agenciesByIco.[a.id].Add(a)
        let agencyIdMap =
            Seq.concat [
                Seq.zip agenciesToAdd agenciesToAddNewId
                |> Seq.map (fun (a, ani) ->
                    (a.id, a.idDistinction), (ani.id, ani.idDistinction))

                existingAgencies
                |> Seq.map (fun a ->
                    let existing = agenciesByName.[a.name]
                    (a.id, a.idDistinction),
                    (existing.id, existing.idDistinction))
            ]
            |> Map
            
        // For now we don't resolve validity overlaps and leave that for a
        // post-processing phase
        let newRoutes =
            batch.routes
            |> Seq.map (fun r ->
                let notFirst, other = routesByLicNum.TryGetValue(r.id)
                let other = if notFirst then other else ResizeArray()
                let aid, aidd = agencyIdMap.[(r.agencyId, r.agencyDistinction)]
                {r with
                    idDistinction = (Seq.length other) + 1
                    agencyId = aid
                    agencyDistinction = aidd})
        for r in newRoutes do
            if not <| routesByLicNum.ContainsKey(r.id) then
                routesByLicNum.[r.id] <- ResizeArray()
            routesByLicNum.[r.id].Add(r)
        let routeIdMap =
            Seq.zip batch.routes newRoutes
            |> Seq.map (fun (r, rni) ->
                (r.id, r.idDistinction), (rni.id, rni.idDistinction))
            |> Map
            
        routeIntegrations.AddRange(
            batch.routeIntegrations
            |> Seq.map (fun ri ->
                let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
                { ri with
                    routeId = rid
                    routeDistinction = ridd
                }))

        routeStops.AddRange(
            batch.routeStops
            |> Seq.map (fun rs ->
                let rid, ridd = routeIdMap.[(rs.routeId, rs.routeDistinction)]
                { rs with
                    routeId = rid
                    routeDistinction = ridd
                    stopId = stopIdMap.[rs.stopId]
                }))

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

        trips.AddRange(
            batch.trips
            |> Seq.map (fun t ->
                let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
                { t with
                    routeId = rid
                    routeDistinction = ridd
                    tripGroupId =
                        t.tripGroupId
                        |> Option.map (fun id -> tripGroupIdMap.[id])
                }))
        
        tripStops.AddRange(
            batch.tripStops
            |> Seq.map (fun ts ->
                let rid, ridd = routeIdMap.[(ts.routeId, ts.routeDistinction)]
                { ts with
                    routeId = rid
                    routeDistinction = ridd
                }))

        routeInfo.AddRange(
            batch.routeInfo
            |> Seq.map (fun ri ->
                let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
                { ri with
                    routeId = rid
                    routeDistinction = ridd
                }))

        serviceNotes.AddRange(
            batch.serviceNotes
            |> Seq.map (fun sn ->
                let rid, ridd = routeIdMap.[(sn.routeId, sn.routeDistinction)]
                { sn with
                    routeId = rid
                    routeDistinction = ridd
                }))

        transfers.AddRange(
            batch.transfers
            |> Seq.map (fun t ->
                let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
                { t with
                    routeId = rid
                    routeDistinction = ridd
                }))

        agencyAlternations.AddRange(
            batch.agencyAlternations
            |> Seq.map (fun aa ->
                let rid, ridd = routeIdMap.[(aa.routeId, aa.routeDistinction)]
                let aid, aidd = agencyIdMap.[(aa.agencyId, aa.agencyDistinction)]
                { aa with
                    routeId = rid
                    routeDistinction = ridd
                    agencyId = aid
                    agencyDistinction = aidd
                }))

        alternateRouteNames.AddRange(
            batch.alternateRouteNames
            |> Seq.map (fun arn ->
                let rid, ridd = routeIdMap.[(arn.routeId, arn.routeDistinction)]
                { arn with
                    routeId = rid
                    routeDistinction = ridd
                }))
        
        reservationOptions.AddRange(
            batch.reservationOptions
            |> Seq.map (fun ro ->
                let rid, ridd = routeIdMap.[(ro.routeId, ro.routeDistinction)]
                { ro with
                    routeId = rid
                    routeDistinction = ridd
                }))

        ()