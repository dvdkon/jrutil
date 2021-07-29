// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík

open System.Threading
open FSharp.Data
open NodaTime
open Serilog

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open JrUtil.RealTimeModel
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
    --grapp-stops-csv=FILE   CSV file with GRAPP stops
    --golemio-api-key=KEY    Golemio API key (https://api.golemio.cz/api-keys)

GRAPP stops CSV columns (GPS coords as floating point numbers):
    SR70,NÁZEV20,GPS X,GPS Y
"""

let collect connGetter skipStops grappStopsCsv golemioApiKey =
    grappStopsCsv |> Option.iter (fun grappStopsCsv ->
        let conn = connGetter()
        let mutable grappStopsDate = LocalDate(0, 1, 1)
        let mutable grappStopMap = Map []
        new Timer(
            (fun _ ->
                if not skipStops && grappStopsDate <> dateToday () then
                    measureTime "Reading and inserting Grapp stops" (fun () ->
                        grappStopsDate <- dateToday ()
                        let grappStops =
                            Grapp.readStopsCsv grappStopsDate grappStopsCsv
                        grappStopMap <- Grapp.getNameIdMap grappStops
                        RealTimeSql.insertStops conn grappStops
                    )
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
            ), null :> obj, 0, 10*60*1000) |> ignore)

    golemioApiKey |> Option.iter (fun apiKey ->
        let conn = connGetter()
        let mutable stopsDate = LocalDate(0, 1, 1)
        new Timer(
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
                        insertStopHistory conn tripDetails.tripId tripDetails.tripStartDate stopHistory
                    ))
            // Every 30 minutes, fire immediately
            ), null, 0, 30*60*1000) |> ignore)

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
            collect connGetter
                    (argFlagSet args "--skip-stops")
                    (optArgValue args "--grapp-stops-csv")
                    (optArgValue args "--golemio-api-key")
            0
    )
