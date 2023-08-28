// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module provides utility functions that can help repair bad JDF data,
/// intentionally bad or not.
module JrUtil.JdfFixups

open System
open System.Text.RegularExpressions
open NetTopologySuite.Geometries
open Serilog
open Serilog.Events

open JrUtil
open JrUtil.Jdf
open JrUtil.JdfModel
open JrUtil.GeoData.Common
open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.EurTowns
open JrUtil.GeoData.StopMatcher

type GeodataPrecision =
    | StopPrecise
    | TownPrecise

type JdfStopGeodata = {
    regionId: string option
    country: string option
    // The CRS is assumed to be ETRS89-Extended
    point: Point
    precision: GeodataPrecision
}
type JdfStopToMatch = StopToMatch<JdfStopGeodata>

/// Matches that are too spread out (radius > ...) aren't considered for
/// matching
let maxRadiusMetres = 1000.0

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
                country = Some "CZ"
                regionId = Some m.Groups.[2].Value
        }
        else stop

let doubleCommaNameRegex = Regex(@"([^,]+),([^,]*),([^,]*)")
let singleCommaNameRegex = Regex(@"([^,]+), *([^,]+)")

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

// When we know (guess, really) that a batch doesn't use MHD naming, we can
// split stop names with single commas
let normaliseNonMhdStopName (stop: Stop) =
    let scMatch = singleCommaNameRegex.Match(stop.town)
    if stop.district.IsNone && stop.nearbyPlace.IsNone
       && scMatch.Success then
        { stop with
            town = scMatch.Groups.[1].Value
            district = Some scMatch.Groups.[2].Value }
    else stop

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

let addRegionFromMatch (stop: Stop) match_ =
    match stop.regionId, match_ with
    | None, Some m ->
        { stop with regionId = m.data.regionId;
                    country = m.data.country }
    | _ -> stop

let czTownNameMatcher =
    Utils.memoizeVoidFunc
    <| fun () ->
        Utils.logWrappedOp "Creating Czech town name matcher" <| fun () ->
            // Sure, it's a *Stop* matcher, but it works for parts of a stop's
            // name too
            new StopMatcher<_>(
                czechTownsPolygons ()
                |> Seq.map (fun (n, r, p) ->
                    { name = n
                      data = r, p
                    })
                |> Seq.toArray)

let eurTownNameMatcher =
    Utils.memoizeVoidFunc <| fun () ->
        Utils.logWrappedOp "Creating European town name matcher" <| fun () ->
            new StopMatcher<_>(
                eurTownCountries ()
                |> Seq.map (fun r ->
                    { name = r.Name
                      data = r.CountryCode.ToUpper(), r.Lat, r.Lon })
                |> Seq.toArray)

let matchCzTownByNameRaw town =
    let town =
        townSynonyms
        |> Map.tryFind town
        |> Option.defaultValue town
    czTownNameMatcher().matchStop(town)

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
                precision =
                    if matches
                       |> Seq.exists (fun m ->
                           m.stop.data.precision = TownPrecise)
                    then TownPrecise
                    else StopPrecise
            }
        }
        else
            Log.Debug("Not considering match for stop {StopId} since \
                      radius {Radius:n1} is too high",
                      stop.id, radius)
            None

let matchConflictsWithEurCity (m: StopToMatch<JdfStopGeodata>) =
    // Some Czech towns share names with ones with other countries. Since we
    // only have coordinates for Czech stops (for now), we need to check if we
    // aren't possibly matching a foreign town to a Czech stop.
    // Correct matches that seemingly conflict can be added in secondary
    // matching, where distance is considered.
    let town = m.name.Split(",").[0]
    eurTownNameMatcher().matchStop(town)
    |> Seq.exists (fun tm -> tm.score = 1f)

let matchCzTownByName (stop: Stop) =
    matchCzTownByNameRaw stop.town
    |> Array.filter (fun m -> m.score = 1f)
    |> Array.map (fun m ->
        {
            stop = {
                name = m.stop.name
                data = {
                    regionId = Some (fst m.stop.data)
                    country = Some "CZ"
                    point = (m.stop.data |> snd).Centroid
                    precision = TownPrecise
                }
            }
            score = 1.0f
        })

let matchByTopTown (stop: Stop) mo =
    let czMatches () =
        matchCzTownByName stop
        |> Array.groupBy (fun m -> m.stop.name, m.stop.data.regionId)
        |> Array.map snd

    match stop.country, stop.regionId with
    | Some "CZ", None ->
        stop,
        match czMatches () with
        | [| ms |] -> Some (Array.head ms).stop
        | _ -> None
    | None, None ->
        let eurTown =
            eurTownNameMatcher().matchStop(stop.town)
            |> Array.tryFind (fun m -> m.score = 1f)
            |> Option.map (fun m -> m.stop.data)
        match czMatches (), eurTown with
        | [| ms |], None -> stop, Some (Array.head ms).stop
        | [| |], Some (c, lato, lono) ->
            match mo, lato, lono with
            | Some _, _, _ -> stop, mo
            | None, None, None ->
                { stop with country = Some c }, None
            | None, Some lat, Some lon ->
                { stop with country = Some c },
                Some {
                    name = stop.town
                    data = {
                        regionId = None
                        country = Some c
                        point =
                            wgs84Factory.CreatePoint(Coordinate(lon, lat))
                            |> pointWgs84ToEtrs89Ex
                        precision = TownPrecise
                    }
                }
            | _ -> stop, None
        | _ -> stop, mo
    | _ -> stop, mo

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
                              float (abs (km - lkm)) > led
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
                        |> Array.filter (fun m ->
                            let region, poly = m.stop.data
                            Seq.zip tss lastPosKmPerTrip
                            |> Seq.exists (fun (tso, lpo) ->
                                let kmOpt =
                                    tso |> Option.bind (fun ts -> ts.kilometer)
                                match lpo, kmOpt with
                                | Some (_, _, lkm, lp, _), Some km ->
                                    poly.Distance(lp) / 1000.0
                                     < float (abs (km - lkm) + 1m)
                                | _ -> false))
                        |> Array.map (fun m -> m.stop.data |> fst)
                        |> set
                        |> Set.toArray
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

/// Will add matches and regions disambiguated by previous matches' positions
/// and km data from tripStops
let addSecondaryMatches
        tolerance
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
                        let kmDiff = abs (km - lkm)
                        ams
                        |> Seq.filter (fun m ->
                            m.stop.data.point.Distance(lp) / 1000.0
                             < float kmDiff + tolerance)
                        |> Seq.toArray
                        |> topStopMatch s
                    | _ -> mo)
                |> Seq.filter ((<>) mo)
            match Seq.tryHead suggested with
            // Check if all trips agree on this match
            | Some (Some pick as pickOpt) ->
                if Seq.forall ((=) pickOpt) suggested then
                    Log.Debug("Picked position for {StopId} by secondary match \
                               ({Precision})",
                              s.id, pick.data.precision)
                    s, pickOpt, ams
                else
                    Log.Debug("Trips disagreed on secondary match \
                               for {StopId}", s.id)
                    s, mo, ams
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

let townNoPerfectMatch town =
    matchCzTownByNameRaw town
    |> Seq.forall (fun m -> m.score < 1f)
    &&
    eurTownNameMatcher().matchStop(town)
    |> Seq.forall (fun m -> m.score < 1f)

/// Some JDF batches in the public CIS JŘ data don't use the full triple for
/// naming stops, but put everything in the town column. This is often used for
/// intra-town lines, and the stop names don't contain the town name directly.
/// This is a problem for matching these stops, so we need to detect these
/// batches and fix them.
///
/// This funtion looks at MHD-named stops and find the town that matches the
/// most stops
let mhdNamingTownCandidates
        (stopMatcher: StopMatcher<_>)
        (stopsWithMatches: (Stop * StopMatch<JdfStopGeodata> array) array) =
    stopsWithMatches
    |> Seq.filter (fun (s, _) ->
        s.district.IsNone
        && s.nearbyPlace.IsNone)
    |> Seq.collect (fun (s, ms) ->
        // Don't suggest any city if we already have a match
        if ms |> exactMatches s |> Seq.isEmpty |> not then Seq.empty
        else
            // Look at all the possible matches, group them by the town they
            // suggest, and take the best score for each town
            ms
            |> Seq.map (fun m ->
                let town = m.stop.name.Split(",").[0].Trim()
                town, m)
            // Only consider stops that match town + old name perfectly
            // but don't match the bare old name
            |> Seq.filter (fun (t, m) ->
                stopMatcher.nameSimilarity(
                    stopNameToTokens $"{t},{s.town}",
                    stopNameToTokens m.stop.name) = 1.0f
                &&
                stopMatcher.nameSimilarity(
                    stopNameToTokens s.town,
                    stopNameToTokens m.stop.name) < 1.0f)
            |> Seq.groupBy (fun (t, m) -> t)
            |> Seq.map fst)
    // Get composite score for each town suggestion
    |> Seq.countBy id
    |> Seq.sortByDescending (fun (t, c) -> c)
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
        // We need to check if this is actually an MHD name or if the first
        // element is a town
        let mhdMatch =
            matchStopByName stopMatcher mhdAdjustedStop
            |> topStopMatch mhdAdjustedStop
        let townMatches = matchCzTownByNameRaw nameSplit.[0]
        if mhdMatch.IsSome then mhdAdjustedStop
        else if townMatches |> Array.exists (fun m -> m.score = 1f)
        then { stop with
                town = nameSplit.[0]
                district = if nameSplit.Length > 1
                           then Some nameSplit.[1]
                           else None
                nearbyPlace = if nameSplit.Length = 3
                              then Some nameSplit.[2]
                              else None }
        else mhdAdjustedStop

let matchStops stopMatcher tripsMatrix1 tripsMatrix2 stops =
    stops
    |> Array.map (fun s -> s, matchStopByName stopMatcher s)
    // Try to assign the stop-accurate matches to a stop, if they are
    // unambiguous.
    |> Array.map (fun (s, ms) ->
        let tm = topStopMatch s ms
        let townOnly = s.district.IsNone && s.nearbyPlace.IsNone
        if townOnly && tm |> Option.map matchConflictsWithEurCity = Some true
        then s, None, ms
        else if townOnly
           && (s.town
               |> matchCzTownByNameRaw
               |> Seq.exists (fun m -> m.score = 1.0f)) then
            Log.Debug("Not considering stop matches for stop {StopId} since \
                      its only name component matches a Czech town",
                      s.id)
            s, None, ms
        else s, tm, ms)
    |> addSecondaryMatches 1.0 tripsMatrix1
    |> addSecondaryMatches 1.0 tripsMatrix2
    // Try matching stops without an existing match to an unambiguous town
    // by name
    |> Array.map (fun (s, mo, ams) ->
        match mo with
        | None ->
            let s2, mo2 = matchByTopTown s mo
            s2, mo2, ams
        | Some _ -> s, mo, ams)

/// Public JDF batches from CIS JŘ have a lot of problems, which this module
/// tries to fix. This function will run them all and return a (hopefully)
/// valid and usable JDF batch, with stop positions as a bonus.
let fixPublicCisJrBatch (stopMatcher: StopMatcher<JdfStopGeodata>)
                        (jdfBatch: JdfBatch) =
    if jdfBatch.routes.Length <> 1 then
        Log.Error("Expected just one route in batch, got {RouteCount}",
                  jdfBatch.routes.Length)

    let tripsMatrix1, tripsMatrix2 =
        groupedTripsMatrices jdfBatch.routeStops jdfBatch.tripStops

    let stops = jdfBatch.stops |> Array.map moveRegionFromName
    let swamNonMhd =
        stops
        |> Array.map (normaliseStopName >> normaliseNonMhdStopName)
        |> matchStops stopMatcher tripsMatrix1 tripsMatrix2
    let mhdTownCandidates =
        stops
        |> Seq.map normaliseStopName
        |> Seq.map (fun s -> s, matchStopByName stopMatcher s)
        |> Seq.toArray
        |> mhdNamingTownCandidates stopMatcher
    let stopsWithAllMatches =
        match Array.tryHead mhdTownCandidates with
        | None ->
            Log.Debug("No town candidate for MHD stops, \
                       Proceeding with unmodified stops")
            swamNonMhd
        // The function returns them sorted already, so first is best
        | Some (town, _) ->
            Log.Debug("Trying candidate town for MHD naming: {Town}", town)
            let swamMhd =
                jdfBatch.stops
                |> Array.map (fixMhdNaming stopMatcher town)
                |> matchStops stopMatcher tripsMatrix1 tripsMatrix2
            let matchCount =
                Array.map (fun (_, mo, _) -> if Option.isSome mo then 1 else 0)
                >> Array.sum
            let mhdMatchCount = matchCount swamMhd
            let nonMhdMatchCount = matchCount swamNonMhd
            Log.Debug("Comparing MHD stops with {MhdMatches} matches vs. \
                       non-mhd with {NonMhdMatches} matches",
                      mhdMatchCount, nonMhdMatchCount)
            if mhdMatchCount > nonMhdMatchCount + 1
            then swamMhd
            else swamNonMhd

    let ifUnfinished f swm =
        if swm |> Seq.exists (fun (s: Stop, _) ->
            s.country.IsNone || (s.country = Some "CZ" && s.regionId.IsNone))
        then f swm
        else swm

    let augmentedStops =
        stopsWithAllMatches
        // Before now we only considered stop-accurate matches,
        // now add town-accurate matches to the mix.
        |> Array.map (fun (s, mo, ams) ->
            match mo with
            | None -> s, mo, Array.concat [| ams; matchCzTownByName s |]
            | Some _ -> s, mo, ams
        )
        // Try secondary matches again with towns
        |> addSecondaryMatches 10.0 tripsMatrix1
        |> addSecondaryMatches 10.0 tripsMatrix2
        // Write data from whatever matches we have into the stops
        |> Array.map (fun (s, mo, _) -> addRegionFromMatch s mo, mo)
        // Fill in gaps, if possible
        |> ifUnfinished (fillStopRegionsFromPrevious tripsMatrix1)
        |> ifUnfinished (fillStopRegionsFromPrevious (Array.rev tripsMatrix1))
        |> ifUnfinished (fillStopRegionsFromPrevious tripsMatrix2)
        |> ifUnfinished (fillStopRegionsFromPrevious (Array.rev tripsMatrix2))
        |> Seq.toArray

    { jdfBatch with stops = augmentedStops |> Array.map fst },
    augmentedStops |> Array.map snd

/// WARN: Expects tripsStops to be for one trip only and sorted by call order
let checkMatchDistances
        (tripStops: TripStop array)
        (stopsWithMatches: (Stop * JdfStopToMatch option) array) =
    let international =
        stopsWithMatches
        |> Array.tryFind (fun (s, _) ->
            s.country
            |> Option.map (fun c -> c <> "CZ")
            |> Option.defaultValue false)
        |> Option.isSome
    let internationalNote =
        if international then " on international route" else ""

    // Some timetables have unrealistically low distance values,
    // I guess nobody checks them...
    let multTolerance = 2.0
    let preciseTolerance = 4m
    let impreciseTolerance = 20m

    let mutable lastPos = None
    let mutable lastPosStopId = None
    let mutable lastPosKm = None
    let mutable lastPrecision = None
    Utils.innerJoinOn (fun (ts: TripStop) -> ts.stopId)
                      (fun (s: Stop, m) -> s.id)
                      tripStops stopsWithMatches
    |> Seq.choose (fun (tripStop, (stop, matchOpt)) ->
        let warning =
            match lastPos, lastPosStopId, lastPosKm, matchOpt,
                  tripStop.kilometer with
            | Some (lp: Point), Some lpsi, Some lkm, Some m, Some km ->
                let dist = lp.Distance(m.data.point) / 1000.0
                let kmDiff = abs (km - lkm)
                let tolerance =
                    if lastPrecision = Some StopPrecise
                       && m.data.precision = StopPrecise
                    then preciseTolerance
                    else impreciseTolerance
                if dist > float (kmDiff + tolerance) * multTolerance
                then Some <| Utils.logEvent
                      LogEventLevel.Warning
                      ("Distance between matches is too high"
                       + internationalNote
                       + " (got {MatchDist}, expected {ExpKm} between \
                          {StopId1} and {StopId2}, {Pos1} and {Pos2})")
                      [|box dist; kmDiff; lpsi; stop.id; lp; m.data.point|]
                else None
            | _ -> None

        match matchOpt, tripStop.kilometer with
        | Some m, Some km ->
            lastPos <- Some m.data.point
            lastPosStopId <- Some stop.id
            lastPosKm <- Some km
            lastPrecision <- Some m.data.precision
        | _ -> ()

        warning
    )

let checkMissingRegionsCountries (jdfBatch: JdfBatch) =
    let isUsed stopId =
        jdfBatch.tripStops
        |> Array.tryFind (fun ts ->
            ts.stopId = stopId
            && match ts.arrivalTime, ts.departureTime with
               | Some (StopTime _), _ -> true
               | _, Some (StopTime _) -> true
               | _ -> false)
        |> Option.isSome
    let level stopId =
        if isUsed stopId
        then LogEventLevel.Warning
        else LogEventLevel.Debug

    jdfBatch.stops
    |> Seq.choose (fun s ->
        match s.country, s.regionId with
        | None, None ->
            Some <| Utils.logEvent
                (level s.id)
                ((if isUsed s.id then "Stop" else "Unused stop")
                 + " without country: {StopId} ({StopName})")
                [|box s.id; jdfStopNameString s |]
        | Some "CZ", None ->
            Some <| Utils.logEvent
                (level s.id)
                ((if isUsed s.id then "Czech stop" else "Unused Czech stop")
                 + " without region: {StopId} ({StopName})")
                [|box s.id; jdfStopNameString s |]
        | _ -> None)
