// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2024 David Koňařík

open System
open System.Threading
open System.Threading.Tasks
open System.Net.Http
open NodaTime
open Serilog

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open JrUtil.RealTimeSql

let docstring = (fun (s: string) -> s.Trim()) """
RtCollect, a long-running program for collecting real-time data from various
sources into a unified database

Usage:
    rtcollect [options] <db-connstr>

Options:
    --logfile=FILE           Path to logfile
    --create-tables          Create tables in the DB and exit
    --skip-stops             Skip inserting stops into DB
    --grapp-stops-csv=FILE   CSV file with GRAPP stops (also for szmapa)
    --golemio-api-key=KEY    Golemio API key (https://api.golemio.cz/api-keys)
    --grapp                  Enable GRAPP scraper
    --szmapa                 Enable mapy.spravazeleznic.cz scraper
    --golemio                Enable Golemio scraper

GRAPP stops CSV columns (GPS coords as floating point numbers):
    SR70,NÁZEV20,GPS X,GPS Y
"""

let collect connGetter args =
    let skipStops = argFlagSet args "--skip-stops"

    // We need to keep references to the timers so they don't get GC'd
    let mutable grappStopMap = Map []
    let grappStopsSignal = new ManualResetEvent(false);
    let _grappStopsTimer = (optArgValue args "--grapp-stops-csv")
                           |> Option.map (fun grappStopsCsv ->
        new Timer(
            (fun _ ->
                if not skipStops then
                    measureTime "Reading and inserting Grapp stops" (fun () ->
                        use conn = connGetter()
                        let grappStops =
                            Grapp.readStopsCsv (dateToday ()) grappStopsCsv
                        grappStopMap <- Grapp.getNameIdMap grappStops
                        RealTimeSql.insertStops conn grappStops
                    )
                    grappStopsSignal.Set() |> ignore
            // Every 24 hours, fire immediately
            ), null, 0, 24*60*60*1000))

    let _grappTimer =
        if not <| argFlagSet args "--grapp" then None else
            let conn = connGetter()
            Some <| new Timer(
                (fun _ ->
                    let indexData = Grapp.fetchIndexData ()
                    let positions =
                        measureTime "Getting train positions from Grapp" (fun () ->
                            Grapp.fetchAllTrains indexData grappStopMap ()
                            |> Seq.toArray)
                    // TODO: Maybe "invert" the data first and the make 2 big
                    // INSERT/COPY calls?
                    measureTime "Inserting train positions from Grapp into DB" (fun () ->
                        positions |> Array.iter (insertTripPosition conn))
                // Every 10 minutes, fire immediately
                ), null, 0, 10*60*1000)

    if argFlagSet args "--szmapa" then
        measureTime "Waiting for stops list to load" (fun () ->
            grappStopsSignal.WaitOne() |> ignore)

        let conn = connGetter()
        let httpClient = new HttpClient()
        httpClient.Timeout <- TimeSpan.FromSeconds(15)
        let scraper = SzMapa.TrainPositionScraper(grappStopMap)

        let timer = new PeriodicTimer(TimeSpan.FromSeconds(10)) // Every 10 seconds
        ignore <| backgroundTask {
            while! timer.WaitForNextTickAsync() do
                try
                    scraper.setStopNameIdMap grappStopMap
                    let! resp =
                        measureTimeAsync "Fetching train positions from IM SŽ" (
                            SzMapa.fetchTrains httpClient)
                    let stopHistory, tripDetails =
                        measureTime "Processing response from IM SŽ" (fun () ->
                            scraper.processResponse(resp))
                    Log.Information("Have {EntryCount} new stop history entries",
                                    stopHistory.Count)
                    tripDetails
                    |> Seq.groupBy (fun td -> td.tripId, td.tripStartDate)
                    |> Seq.iter (fun (key, xs) ->
                        if Seq.length xs > 1 then
                            Log.Error("Multiple entries for {Key}: {Entries}", key, xs))
                    measureTime "Inserting results from IM SŽ into DB" (fun () ->
                        insertStopHistory conn stopHistory
                        insertTripDetails conn tripDetails
                    )
                with e ->
                    Log.Error(e, "Failed scraping IM SŽ")
        }

    let _golemioTimer =
        if not <| argFlagSet args "--golemio" then None else
            let apiKey = argValue args "--golemio-api-key"
            let conn = connGetter()
            let mutable stopsDate = LocalDate(0, 1, 1)
            Some <| new Timer(
                (fun _ ->
                    if not skipStops && stopsDate <> dateToday () then
                        measureTime "Getting and inserting Golemio PID stops" (fun () ->
                            stopsDate <- dateToday ()
                            Golemio.getStops apiKey ()
                            |> RealTimeSql.insertStops conn
                        )
                    measureTime "Getting and inserting positions from GolemIO into DB" (fun () ->
                        Golemio.getPositions apiKey true ()
                        |> Seq.iter (fun (tripDetails, coordHistory, stopHistory) ->
                            insertTripDetails conn [tripDetails]
                            insertCoordHistory conn coordHistory
                            insertFullStopHistory conn tripDetails.tripId
                                                  tripDetails.tripStartDate
                                                  stopHistory
                        ))
                // Every 10 minutes, fire immediately
                ), null, 0, 10*60*1000)

    Thread.Sleep(Timeout.Infinite)

[<EntryPoint>]
let main argv =
    withProcessedArgs docstring argv (fun args ->
        setupLogging (optArgValue args "--logfile") ()
        Log.Information("RtCollect started!")
        let connGetter = fun () ->
            let conn = getPostgresqlConnection (argValue args "<db-connstr>")
            setSchema conn "rtcollect"
            conn

        if (argFlagSet args "--create-tables") then
            let conn = connGetter ()
            cleanAndSetSchema conn "rtcollect"
            sqlCreateRealTimeTables conn
            Log.Information("RtCollect tables created!")
            0
        else
            collect connGetter args
            0
    )
