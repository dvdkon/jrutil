// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module provides utility functions that can help repair bad JDF data,
/// intentionally bad or not.
module JrUtil.JdfFixups

open System
open System.Text.RegularExpressions
open NetTopologySuite.Geometries
open Serilog

open JrUtil
open JrUtil.Jdf
open JrUtil.JdfModel
open JrUtil.GeoData.Common
open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.EurTowns
open JrUtil.GeoData.StopMatcher

type JdfStopGeodata = {
    regionId: string option
    country: string option
    // The CRS is assumed to be ETRS89-Extended
    point: Point
}
type JdfStopToMatch = StopToMatch<JdfStopGeodata>

/// Matches that are too spread out (radius > ...) aren't considered for
/// matching
let maxRadiusMetres = 500.0

/// Some JDF batches in CIS JŘ public exports have the nearby town two-letter id
/// appended to the town name like so: "Praha [AB]". This function will move it
/// to its rightful place three columns to the right.
let moveRegionFromName =
    let ntRegex = Regex(@"(.*) \[(..)\]$")
    fun (stop: Stop) ->
        let m = ntRegex.Match(stop.town)
        if m.Success
        then {
            stop with
                town = m.Groups.[1].Value
                regionId = Some m.Groups.[2].Value
        }
        else stop

let doubleCommaNameRegex = Regex(@"([^,]+),([^,]*),([^,]*)")

/// In the public CIS JŘ exports, stops don't have consistent naming.
/// Sometimes, the whole name, delimited by commas, is in "town".
/// When "districy" should be empty, it sometimes has the value of
/// "nearbyPlace".
/// This function will take a Stop and give it a normalised name that can
/// later be compared between batches, not necessarily so some yet-unpublished
/// official stop registry name.
let normaliseStopName (stop: Stop) =
    let emptyToNone s = if s = "" then None else Some s
    let newTown, newDistrict, newNearbyPlace =
        let dcMatch = doubleCommaNameRegex.Match(stop.town)
        if stop.district.IsNone && stop.nearbyPlace.IsNone
           && dcMatch.Success then
            dcMatch.Groups.[1].Value,
            emptyToNone dcMatch.Groups.[2].Value,
            emptyToNone dcMatch.Groups.[3].Value
        else if stop.district.IsNone then
            stop.town, stop.nearbyPlace, None
        else
            stop.town, stop.district, stop.nearbyPlace
    { stop with
        town = newTown
        district = newDistrict
        nearbyPlace = newNearbyPlace }

let matchStopByName (matcher: StopMatcher<JdfStopGeodata>)
                    (stop: Stop) =
    matcher.matchStop(jdfStopNameString stop)

let exactMatches (stop: Stop) (matches: StopMatch<JdfStopGeodata> array) =
    matches
    |> Array.filter (fun m ->
        // Only take perfect matches
        m.score = 1.0f
        // If the stop has a region assigned, honour it
        && match stop.regionId with
           | Some r -> Some r = m.stop.data.regionId
           | None -> true
        // Same for the country
        && match stop.country with
           | Some c -> Some c = m.stop.data.country
           | None -> true)

let topStopMatch (stop: Stop) (matches: StopMatch<JdfStopGeodata> array) =
    let matches = exactMatches stop matches

    if matches.Length = 0 then None
    // Sanity check - if stops are close together, they need to be in the same
    // region and country
    else if matches
            |> Array.map (fun m -> m.stop.data.regionId, m.stop.data.country)
            |> set
            |> Set.count > 1 then None
    else
        let points = matches |> Array.map (fun m -> m.stop.data.point)
        let radius = pointsRadius points
        if radius < maxRadiusMetres
        then Some {
            name = matches.[0].stop.name
            data = {
                regionId = matches.[0].stop.data.regionId
                country = matches.[0].stop.data.country
                point = GeometryCollection(
                            points |> Array.map (fun p -> p :> _)
                        ).Centroid
            }
        }
        else
            Log.Debug("Not considering match for stop {Stop} since \
                      radius {Radius:n1} is too high",
                      jdfStopNameString stop, radius)
            None

let addStopRegionsFromMatch =
    Seq.map (fun (stop: Stop, match_) ->
        match stop.regionId, match_ with
        | None, Some m ->
            { stop with regionId = m.data.regionId;
                        country = m.data.country }, match_
        | _ -> stop, match_)

let czTownNameMatcher =
    Utils.memoizeVoidFunc
    <| fun () ->
        Utils.logWrappedOp "Creating Czech town name matcher" <| fun () ->
            // Sure, it's a *Stop* matcher, but it works for parts of a stop's
            // name too
            new StopMatcher<_>(
                townsWithRegions().Rows
                |> Seq.map (fun r ->
                    { name = r.Name; data = r.RegionId })
                |> Seq.toArray)

let addStopRegionsFromCzTownName ss =
    ss |> Seq.map (fun (stop: Stop, m) ->
        match stop.regionId, stop.country with
        | None, None | None, Some "CZ" ->
            let rs = czTownNameMatcher().matchStop(stop.town)
                     |> Array.filter (fun m -> m.score = 1f)
                     |> Array.map (fun m -> m.stop.data)
                     |> set
                     |> Set.toArray
            match rs with
            | [| r |] ->
                { stop with regionId = Some r; country = Some "CZ" }, m
            | _ -> stop, m
        | _ -> stop, m)

let eurTownNameMatcher =
    Utils.memoizeVoidFunc <| fun () ->
        Utils.logWrappedOp "Creating European town name matcher" <| fun () ->
            new StopMatcher<_>(
                eurTownCountries ()
                |> Seq.map (fun r -> { name = r.Name;
                                       data = r.CountryCode.ToUpper() })
                |> Seq.toArray)

let addStopCountriesFromEurTownName ss =
    ss |> Seq.map (fun (stop: Stop, m) ->
        match stop.country with
        | Some _ -> stop, m
        | None ->
            let c = eurTownNameMatcher().matchStop(stop.town)
                    |> Array.tryFind (fun m -> m.score = 1f)
                    |> Option.map (fun m -> m.stop.data)
            { stop with country = c }, m)

let distanceToCzRegionEdge (stop1Pos: Point) stop2RegionId =
    let region = (czechRegionPolygons ()).[stop2RegionId]
    stop1Pos.Distance(region.Shell)

// tripStops in a trip are assumed to be sorted by the order they appear on the
// line (one direction only!) and to be for one specific line
let fillStopRegionsFromPrevious
        (tripsMatrix: TripStop option array array)
        stopsWithMatches =
    let mutable lastRegion = None
    let mutable lastCountry = None
    // Tuple of country, region, km, position, distance to region edge
    let mutable lastPosKmPerTrip =
        tripsMatrix
        |> Array.tryHead
        |> Option.defaultValue [||]
        |> Seq.map (fun _ -> None)
        |> Seq.toArray

    let augmentedStops =
        Utils.innerJoinOn (fun (tss: TripStop option array) ->
                              (tss |> Seq.choose id |> Seq.head).stopId)
                          (fun (s: Stop, _) -> s.id)
                          tripsMatrix stopsWithMatches
        |> Seq.map (fun (tss, (s, mo)) ->
            let newStop =
                match s.regionId, s.country with
                // Stop has country and region, no need to change, update prev
                | Some r, Some c ->
                    lastRegion <- Some r
                    lastCountry <- Some c
                    s
                | None, Some c ->
                    if Some c = lastCountry then
                        // Stop has country only and previous country matches,
                        // add region
                        {s with regionId = lastRegion}
                    else
                        // Stop has country only and previous country doesn't
                        // match, don't modify
                        lastRegion <- None
                        lastCountry <- Some c
                        s
                // Stop has region only, add country
                | Some r, None ->
                    lastRegion <- Some r
                    {s with country = lastCountry}
                // Stop has nothing, add region and country
                | None, None ->
                    {s with regionId = lastRegion; country = lastCountry}

            let newStop =
                if newStop.regionId <> s.regionId
                   && Seq.zip tss lastPosKmPerTrip
                      |> Seq.forall (fun (ts, lpo) ->
                          let kmOpt =
                              ts |> Option.bind (fun ts -> ts.kilometer)
                          match lpo, kmOpt with
                          | Some (lc, lr, lkm, _, led), Some km
                            when Some lc = newStop.country
                              && Some lr = newStop.regionId ->
                              float (km - lkm) > led
                          | _ -> true)
                // If we changed the region but our change is uncertain,
                // revert it
                then
                    Log.Debug("Not setting region for {StopId} from previous \
                               stop since distance is too large \
                               (could cross region boundary)", s.id)
                    s
                else
                    newStop

            let newStop =
                if [ None; Some "CZ" ] |> Seq.contains newStop.country
                   && newStop.regionId.IsNone then
                    // Try to find region by looking at possible towns
                    let regions =
                        czTownNameMatcher().matchStop(s.town)
                        |> Array.filter (fun m -> m.score = 1f)
                        |> Array.map (fun m -> m.stop.data)
                        |> Array.filter (fun r ->
                            let poly = czechRegionPolygons().[r]
                            Seq.zip tss lastPosKmPerTrip
                            |> Seq.exists (fun (tso, lpo) ->
                                let kmOpt =
                                    tso |> Option.bind (fun ts -> ts.kilometer)
                                match lpo, kmOpt with
                                | Some (_, _, lkm, lp, _), Some km ->
                                    poly.Distance(lp) / 1000.0
                                     < float (km - lkm + 1m)
                                | _ -> false))
                    match regions with
                    | [| r |] ->
                        Log.Debug("Picked region by distance \
                                   for {StopId}: {Region}",
                                  newStop.id, r)
                        { newStop with country = Some "CZ"; regionId = Some r}
                    | rs ->
                        Log.Debug("Can't pick region by distance \
                                   for {StopId} from {Regions}",
                                  s.id, rs)
                        newStop
                else newStop

            for i, ts in Seq.indexed tss do
                let kmOpt = ts |> Option.bind (fun ts -> ts.kilometer)
                match mo, kmOpt, s.country, s.regionId with
                | Some m, Some km, Some c, Some r when c = "CZ" ->
                    let edgeDist =
                        distanceToCzRegionEdge m.data.point r / 1000.0
                    lastPosKmPerTrip.[i] <-
                        Some (c, r, km, m.data.point, edgeDist)
                | _ -> ()

            newStop, mo
        )
        // Stops can occur multiple times, pick the one with the most
        // information
        |> Seq.groupBy (fun (s: Stop, _) -> s.id)
        |> Seq.map (fun (_, xs) ->
            xs
            |> Seq.sortByDescending (fun (s, _) ->
                s.regionId.IsSome, s.country.IsSome)
            |> Seq.head)
        |> Seq.toArray

    let augmentedStopIds =
        augmentedStops |> Array.map (fun (s, _) -> s.id) |> set
    // Add back in the stops that weren't used in any of the trips
    Array.concat [
        augmentedStops

        stopsWithMatches
        |> Array.filter (fun (s, _) ->
            augmentedStopIds |> Set.contains s.id |> not)
    ]

/// Sort tripStops for one trip by call order
let sortOneTripStops =
    Array.sortBy (fun (ts: TripStop) ->
        ts.routeStopId * if ts.tripId % 2L = 1L then 1L else -1L)

// Takes trip stops and grous them twice: by direction and then by pattern,
// creating two matrices of tripStops (row - one stop)
let groupedTripsMatrices (routeStops: RouteStop array)
                         (tripStops: TripStop array) =
    let routeStopIds =
        routeStops
        |> Array.map (fun rs -> rs.routeStopId)
        |> Array.sort
    let trips = tripStops |> Array.groupBy (fun ts -> ts.tripId)
    let tripsByRem rem =
        trips
        |> Array.filter (fun (tid, ts) -> tid % 2L = rem)
        |> Array.map (fun (_, ts) ->
            Utils.leftJoinOn id (fun (ts: TripStop) -> ts.routeStopId)
                             routeStopIds ts
            |> Seq.map snd
            |> Seq.toArray)
        // Take one trip per physical route
        |> Array.groupBy (Array.map (Option.map (fun t -> t.kilometer)))
        |> Array.map (snd >> Array.head
                      >> (if rem = 0L then Array.rev else id))
        // Transpose to have stops as rows
        |> Array.transpose
        |> Array.filter (Array.exists Option.isSome)

    tripsByRem 1L, tripsByRem 0L

// routeStops and tripStops must be for one route only
let addRegions trips
               (stopsWithMatches: (Stop * JdfStopToMatch option) array) =
    let augmented =
        stopsWithMatches
        |> addStopRegionsFromMatch
        |> addStopRegionsFromCzTownName
        |> addStopCountriesFromEurTownName
        |> Seq.toArray
    // Check if there is still a stop which needs a region assigned
    if augmented
       |> Array.exists (fun (s, _) ->
           s.regionId.IsNone
           && [|Some "CZ"; None|] |> Array.contains s.country) then
        let forwardTrips, backwardTrips = trips

        augmented
        |> fillStopRegionsFromPrevious forwardTrips
        |> fillStopRegionsFromPrevious backwardTrips
    else augmented

/// Will add matches and regions disambiguated by previous matches' positions
/// and km data from tripStops
let addSecondaryMatches
        (tripsMatrix: TripStop option array array)
        // Array of tuples of:
        // - JDF stop
        // - previously confirmed match
        // - all matches
        (stopsWithMatches: (Stop
                            * JdfStopToMatch option
                            * StopMatch<JdfStopGeodata> array) array) =
    // Tuple of position, km
    let mutable lastPosKmPerTrip =
        tripsMatrix
        |> Array.tryHead
        |> Option.defaultValue [||]
        |> Seq.map (fun _ -> None)
        |> Seq.toArray

    let augmentedStops =
        Utils.innerJoinOn (fun (tss: TripStop option array) ->
                              (tss |> Seq.choose id |> Seq.head).stopId)
                          (fun (s: Stop, _, _) -> s.id)
                          tripsMatrix stopsWithMatches
        |> Seq.map (fun (tss, (s, mo, ams)) ->
            let suggested =
                tss
                |> Seq.mapi (fun i ts ->
                    let kmOpt = ts |> Option.bind (fun ts -> ts.kilometer)
                    match mo, kmOpt, lastPosKmPerTrip.[i] with
                    | Some m, Some km, _ ->
                        lastPosKmPerTrip.[i] <- Some (m.data.point, km)
                        Some m
                    | None, Some km, Some (lp, lkm) ->
                        let kmDiff = km - lkm
                        ams
                        |> Seq.filter (fun m ->
                            m.stop.data.point.Distance(lp) / 1000.0
                             < float kmDiff + 1.0)
                        |> Seq.toArray
                        |> topStopMatch s
                    | _ -> mo)
                |> Seq.filter ((<>) mo)
            match Seq.tryHead suggested with
            // Check if all trips agree on this match
            | Some pick when Seq.forall ((=) pick) suggested ->
                s, pick, ams
            | _ -> s, mo, ams)
        |> Seq.groupBy (fun (s: Stop, _, _) -> s.id)
        |> Seq.map (fun (_, xs) ->
            xs
            |> Seq.sortByDescending (fun (_, mo, _) -> mo.IsSome)
            |> Seq.head)
        |> Seq.toArray
    let augmentedStopIds =
        augmentedStops |> Array.map (fun (s, _, _) -> s.id) |> set
    Array.concat [
        augmentedStops

        stopsWithMatches
        |> Array.filter (fun (s, _, _) ->
            augmentedStopIds |> Set.contains s.id |> not)
    ]

/// WARN: Expects tripsStops to be for one trip only and sorted by call order
let checkMatchDistances
        (tripStops: TripStop array)
        (stopsWithMatches: (Stop * JdfStopToMatch option) array) =
    let mutable lastPos = None
    let mutable lastPosKm = None
    Utils.innerJoinOn (fun (ts: TripStop) -> ts.stopId)
                      (fun (s: Stop, m) -> s.id)
                      tripStops stopsWithMatches
    |> Seq.map (fun (tripStop, (stop, matchOpt)) ->
        let warning =
            match lastPos, lastPosKm, matchOpt, tripStop.kilometer with
            | Some (lp: Point), Some lkm, Some m, Some km ->
                let dist = lp.Distance(m.data.point) / 1000.0
                if dist > float (km - lkm + 2m)
                then Some <| "Distance between matches is too high "
                   + sprintf "(got %f, expected %M between %A and %A)"
                             dist (km - lkm ) lastPos m.data.point
                else None
            | _ -> None

        match matchOpt, tripStop.kilometer with
        | Some m, Some km ->
            lastPos <- Some m.data.point
            lastPosKm <- Some km
        | _ -> ()

        warning
    )
    |> Seq.choose id
    |> Seq.toArray

/// WARN: Expects tripsStops to be for one trip only and sorted by call order
/// or in reverse (first stop first or last)
let checkRegionDistances
        (tripStops: TripStop array)
        (stopsWithMatches: (Stop * JdfStopToMatch option) array) =
    let mutable lastEdgeDist = None
    let mutable lastPosKm = None
    let mutable lastRegion = None
    Utils.innerJoinOn (fun (ts: TripStop) -> ts.stopId)
                      (fun (s: Stop, m) -> s.id)
                      tripStops stopsWithMatches
    |> Seq.map (fun (tripStop, (stop, matchOpt)) ->
        let warning =
            match matchOpt, lastEdgeDist, lastPosKm, lastRegion,
                  stop.regionId, tripStop.kilometer with
            | None, Some ekm, Some lpkm, Some lr, Some r, Some km ->
                let dist = abs (km - lpkm)
                if lr = r && float (dist - 2m) > ekm
                then Some <| sprintf "Distance %M between last match \
                                      and stop %d is too high \
                                      (could cross region boundary %f away)"
                                     dist stop.id ekm
                else None
            | _ -> None

        match matchOpt, tripStop.kilometer, stop.regionId with
        | Some m, Some km, Some r ->
            lastEdgeDist <- Some (distanceToCzRegionEdge m.data.point r / 1000.0)
            lastPosKm <- Some km
            lastRegion <- Some r
        | _ -> ()

        warning
    )
    |> Seq.choose id
    |> Seq.toArray

/// Some JDF batches in the public CIS JŘ data don't use the full triple for
/// naming stops, but put everything in the town column. This is often used for
/// intra-town lines, and the stop names don't contain the town name directly.
/// This is a problem for matching these stops, so we need to detect these
/// batches and fix them.
/// Town-only stops with proper naming are very rare, so this shouldn't catch
/// any non-MHD batch
let hasMhdNaming stops =
    let townOnlyStops =
        stops
        |> Array.filter (fun (s: Stop) -> s.district.IsNone
                                       && s.nearbyPlace.IsNone)
    let doubleCommaStops =
        townOnlyStops
        |> Array.filter (fun s -> s.town.Split(",").Length = 3)
    let likelyMhdStops =
        townOnlyStops
        |> Array.filter (fun s ->
            czTownNameMatcher().matchStop(s.town)
            |> Seq.forall (fun m -> m.score < 1f)
            &&
            eurTownNameMatcher().matchStop(s.town)
            |> Seq.forall (fun m -> m.score < 1f))

    // Sadly, we have to resort to guessing based on the majority here
    townOnlyStops.Length > stops.Length / 2
    && doubleCommaStops.Length <> townOnlyStops.Length
    && likelyMhdStops.Length > townOnlyStops.Length / 2


/// Look at MHD-named stops and find the town that matches the most stops
/// TODO: Cities with longer names are penalised because of the scoring
/// mechanism used by StopMatcher.
let mhdNamingTownCandidates
        (stopsWithMatches: (Stop * StopMatch<JdfStopGeodata> array) array) =
    stopsWithMatches
    |> Seq.filter (fun (s, _) -> s.district.IsNone && s.nearbyPlace.IsNone)
    |> Seq.collect (fun (s, ms) ->
        // Look at all the possible matches, group them by the town they
        // suggest, and take the best score for each town
        ms
        |> Seq.map (fun m ->
            let town = m.stop.name.Split(",").[0].Trim()
            town, m.score)
        |> Seq.groupBy (fun (t, sc) -> t)
        |> Seq.map (fun (t, es) ->
            t, (es |> Seq.map (fun (t, sc) -> sc)) |> Seq.max))
    // Get composite score for each town suggestion
    |> Seq.groupBy (fun (t, sc) -> t)
    |> Seq.map (fun (t, es) -> es |> Seq.map (fun (_, s) -> s) |> Seq.sum, t)
    |> Seq.sortDescending
    |> Seq.toArray

let fixMhdNaming (stopMatcher: StopMatcher<JdfStopGeodata>) town (stop: Stop) =
    if stop.district.IsSome || stop.nearbyPlace.IsSome then stop
    else
        let nameSplit = stop.town.Split(",")
        if nameSplit.Length > 3 then
            Log.Warning("MHD-named stop with more than two commas: {StopName}",
                        stop.town)
        let nameSplit12 = String.Join(" ", nameSplit.[1..]).Trim()
        let mhdAdjustedStop =
            { stop with
                town = town
                district = Some nameSplit.[0]
                nearbyPlace =
                    if nameSplit12 <> ""
                    then Some nameSplit12
                    else None }
        if nameSplit.Length > 1 then
            // We need to check if this is actually an MHD name or if the first
            // element is a town
            let mhdMatch =
                matchStopByName stopMatcher mhdAdjustedStop
                |> topStopMatch mhdAdjustedStop
            let townMatches = (czTownNameMatcher ()).matchStop(nameSplit.[0])
            if mhdMatch.IsSome then mhdAdjustedStop
            else if townMatches |> Array.exists (fun m -> m.score = 1f)
            then { stop with
                    town = nameSplit.[0]
                    district = Some nameSplit.[1]
                    nearbyPlace = if nameSplit.Length = 3
                                  then Some nameSplit.[2]
                                  else None }
            else mhdAdjustedStop
        else mhdAdjustedStop


/// Public JDF batches from CIS JŘ have a lot of problems, which this module
/// tries to fix. This function will run them all and return a (hopefully)
/// valid and usable JDF batch, with stop positions as a bonus.
let fixPublicCisJrBatch (stopMatcher: StopMatcher<JdfStopGeodata>)
                        (jdfBatch: JdfBatch) =
    let stops =
        if hasMhdNaming jdfBatch.stops then
            let stopsWithMatches =
                jdfBatch.stops
                |> Seq.map normaliseStopName
                |> Seq.map (fun s -> s, matchStopByName stopMatcher s)
                |> Seq.toArray
            let townCandidates = mhdNamingTownCandidates stopsWithMatches
            match Array.tryHead townCandidates with
            | None -> Log.Error("No town candidate for MHD stops! \
                                 Proceeding with unmodified stops")
                      jdfBatch.stops
            // The function retunrns them sorted already, so first is best
            | Some (_, town) ->
                Log.Debug("Processing with MHD naming, with town {Town}", town)
                jdfBatch.stops |> Array.map (fixMhdNaming stopMatcher town)
        else
            jdfBatch.stops
            |> Array.map (moveRegionFromName >> normaliseStopName)

    if jdfBatch.routes.Length <> 1 then
        Log.Error("Expected just one route in batch, got {RouteCount}",
                  jdfBatch.routes.Length)

    let tripsMatrix1, tripsMatrix2 =
        groupedTripsMatrices jdfBatch.routeStops jdfBatch.tripStops

    let stopsWithAllMatches =
        stops
        |> Array.map (fun s -> s, matchStopByName stopMatcher s)
    let stopsWithTopMatches =
        stopsWithAllMatches
        |> Array.map (fun (s, ms) -> s, topStopMatch s ms, ms)
        |> addSecondaryMatches tripsMatrix1
        |> addSecondaryMatches tripsMatrix2
        |> Array.map (fun (s, mo, _) -> s, mo)

    let stopsWithRegions =
        addRegions (tripsMatrix1, tripsMatrix2) stopsWithTopMatches
        |> Seq.toArray

    { jdfBatch with stops = stopsWithRegions |> Array.map fst },
    stopsWithRegions |> Array.map snd
