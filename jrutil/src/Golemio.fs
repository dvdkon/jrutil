// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík

module JrUtil.Golemio

open Serilog
open NodaTime
open NodaTime.Text
open FSharp.Data
open JrUtil
open JrUtil.RealTimeSql
open JrUtil.RealTimeModel
open JrUtil.Utils

let baseUrl = "https://api.golemio.cz"
let stopsPageSize = 10000
// Officially can be up to 10000, but that can result in error 500s
let positionsPageSize = 100

type V2GtfsStopsOutput =
    JsonProvider<const(__SOURCE_DIRECTORY__ + "/../samples/golemio_v2_gtfs_stops.json")>
type V2VehiclePositionsOutput =
    JsonProvider<const(__SOURCE_DIRECTORY__ + "/../samples/golemio_v2_vehiclepositions.json"),
                 InferTypesFromValues=false>

let getAllPaginated limit getter =
    Seq.initInfinite (fun i -> i*limit)
    |> Seq.scan (fun lastFeatures offset ->
        match lastFeatures with
        | Some fs when Array.length fs < limit -> None
        | _ -> Some <| getter offset ()
    ) None
    |> Seq.skip 1
    |> Seq.takeWhile Option.isSome
    |> Seq.collect Option.get

let getStops apiKey () =
    getAllPaginated stopsPageSize (fun offset () ->
        let resp =
            Http.RequestString(
                baseUrl + "/v2/gtfs/stops",
                query = ["limit", string stopsPageSize; "offset", string offset],
                headers = ["X-Access-Token", apiKey])
        V2GtfsStopsOutput.Parse(resp).Features
    )
    |> Seq.map (fun gtfsStop -> {
        id = "-PIDS-" + gtfsStop.Properties.StopId
        date = dateToday ()
        name = gtfsStop.Properties.StopName
        lat = Some <| float gtfsStop.Geometry.Coordinates.[1]
        lon = Some <| float gtfsStop.Geometry.Coordinates.[0]
    })

// This outputs the database types directly, since the Golemio API differs from
// all other publically-available APIs in that it offers the complete history
// of position and the "basic" types are made to deal with only one position
// per observation
let processPositions noTrains (trips: V2VehiclePositionsOutput.Feature seq) =
    trips
    |> Seq.filter (fun trip ->
        // Trains seem to have vehicle_type = null while all other trips have
        // vehicle_type not-null (hopefully)
        not (Option.isNone trip.Properties.Trip.VehicleType && noTrains))
    |> Seq.choose (fun trip ->
        try
            let tripProps = trip.Properties.Trip
            // Ideally we'd use CIS JŘ IDs, but cis.line_id is frequently set
            // to "none" even for normal buses
            let tripId = "-PIDT-" + tripProps.Gtfs.TripId
            let routeId = "-PIDT-" + tripProps.Gtfs.RouteId
            let dtPattern =
                LocalDateTimePattern.CreateWithInvariantCulture(
                    "uuuu'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'")
            let parseDt (str: string) = dtPattern.Parse(str).Value
            let startDate = (parseDt tripProps.StartTimestamp).Date
            let tz = DateTimeZoneProviders.Tzdb.GetZoneOrNull("Europe/Prague")
            let routeName = tripProps.Gtfs.RouteShortName

            let tripDetails = {
                tripId = tripId
                tripStartDate = startDate
                routeId = routeId
                shortName =
                    Some <| sprintf "%s (%s)"
                                    routeName
                                    (string tripProps.Cis.TripNumber)
                routeShortName = Some routeName
            }
            let coordHistory =
                trip.Properties.AllPositions.Features
                |> Seq.map (fun pos -> {
                    tripId = tripId
                    tripStartDate = startDate
                    observationTime =
                        (parseDt pos.Properties.OriginTimestamp)
                         .InZoneStrictly(tz)
                         .ToInstant()
                    lat = float pos.Geometry.Coordinates.[1]
                    lon = float pos.Geometry.Coordinates.[0]
                })
                // Evaluate the seqs early so that potential errors get caught
                // by the big overarching `try with`
                |> Seq.toArray
                |> Array.distinctBy (fun chi ->
                    (chi.tripId, chi.tripStartDate, chi.observationTime))
            let stopHistory =
                trip.Properties.AllPositions.Features
                |> Seq.filter (fun pos ->
                    Option.isSome pos.Properties.LastStop.Sequence)
                |> Seq.choose (fun pos ->
                    let lastStop = pos.Properties.LastStop
                    let delay = pos.Properties.Delay
                    let arrival = parseDt lastStop.ArrivalTime.Value
                    let departure = parseDt lastStop.DepartureTime.Value
                    Some {
                        tripId = tripId
                        tripStartDate = startDate
                        stopId = "-PIDS-" + lastStop.Id.Value
                        tripStopIndex = int lastStop.Sequence.Value
                        timeZone = tz
                        arrivedAt =
                            Some <| arrival + Period.FromSeconds(
                                delay.LastStopArrival
                                // The departure delay is also used for
                                // arrival timing, since the arrival delay
                                // seems to only be set for trains
                                |> Option.orElse delay.LastStopDeparture
                                |> Option.defaultValue 0M
                                |> int64)
                        shouldArriveAt = Some arrival
                        departedAt =
                            Some <| departure + Period.FromSeconds(
                                delay.LastStopDeparture
                                |> Option.defaultValue 0M
                                |> int64)
                        shouldDepartAt = Some departure

                    }
                )
                |> Seq.toArray
                |> Array.distinctBy (fun shi -> shi.tripStopIndex)
            Some (tripDetails, coordHistory, stopHistory)
        with e ->
            Log.Error(
                e, "Exception while processing PID trip {TripID} from Golemio",
                trip.Properties.Trip.Gtfs.TripId)
            None
    )

let getPositions apiKey noTrains () =
    getAllPaginated positionsPageSize (fun offset () ->
        let resp =
            Http.RequestString(
                baseUrl + "/v2/vehiclepositions",
                query = [
                    "limit", string positionsPageSize
                    "offset", string offset
                    "includeNotTracking", "true"
                    "includePositions", "true"
                ],
                headers = ["X-Access-Token", apiKey])
        V2VehiclePositionsOutput.Parse(resp).Features
    )
    |> processPositions noTrains
