// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2024 David Koňařík

open System
open System.Net.Http
open System.Reflection
open System.Threading
open NodaTime
open Serilog
open UserAgents

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open JrUtil.RealTimeSql
open System.Net

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
    --ua-comment=COMMENT     Comment to append to user agent
    --ua=VALUE               Replacement User-Agent header value
    --ua-random              Choose random user agent
    --proxy=PROXYLIST        Comma-separated list of proxies to use

GRAPP stops CSV columns (GPS coords as floating point numbers):
    SR70,NÁZEV20,GPS X,GPS Y
"""

type RandomProxy(proxies: Uri array) =
    let rand = Random()
    let credsCache = CredentialCache()
    do
        for proxy in proxies do
            let uis = proxy.UserInfo.Split(':', 2)
            if uis.Length = 2 then
                let creds = NetworkCredential(uis[0], uis[1])
                let ub = UriBuilder(proxy)
                ub.UserName <- ""
                ub.Password <- ""
                credsCache.Add(ub.Uri, "Basic", creds)

    interface IWebProxy with
        member this.Credentials
            with get () = credsCache
            and set (_) = failwith "RandomProxy does not allow changing credentials"
        member this.IsBypassed(host) = false
        member this.GetProxy(dest) =
            let idx = rand.Next(proxies.Length)
            proxies[idx]

let uaSelector = new UserAgentSelector()
let newHttpClient args () =
    let httpClient =

        match optArgValue args "--proxy" with
        | None -> new HttpClient()
        | Some proxyStr ->
            let proxies = proxyStr.Split(',') |> Seq.map Uri |> Seq.toArray
            let handler = new HttpClientHandler(Proxy = RandomProxy(proxies))
            new HttpClient(handler)

    match (argFlagSet args "--ua-random", optArgValue args "--ua") with
    | true, _ ->
        let ua = uaSelector.GetRandom()
        Log.Information("Using User-Agent: {UserAgent}", ua.UserAgentString)
        httpClient.DefaultRequestHeaders.UserAgent.ParseAdd(ua.UserAgentString)
    | _, Some ua ->
        httpClient.DefaultRequestHeaders.Add("User-Agent", ua)
    | _, None ->
        httpClient.DefaultRequestHeaders.UserAgent.Add(
            Headers.ProductInfoHeaderValue(
                "JrUtil-RtCollect",
                Assembly.GetExecutingAssembly().GetName().Version.ToString()))
        optArgValue args "--ua-comment" |> Option.iter (fun c ->
            httpClient.DefaultRequestHeaders.UserAgent.Add(
                Headers.ProductInfoHeaderValue(c)))

    httpClient.Timeout <- TimeSpan.FromSeconds(10L)
    httpClient

let scrapeGrapp args grappStopMap conn = task {
    use httpClient = newHttpClient args ()
    let! indexData = Grapp.fetchIndexData httpClient ()
    let! positions =
        measureTimeAsync "Getting train positions from Grapp" (fun () ->
            Grapp.fetchAllTrains httpClient indexData grappStopMap ())
    Log.Information("Got {TrainCount} trains from GRAPP", positions.Length)
    // TODO: Maybe "invert" the data first and the make 2 big
    // INSERT/COPY calls?
    measureTime "Inserting train positions from Grapp into DB" (fun () ->
        positions |> Array.iter (insertTripPosition conn))
}

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
                    measureTime "Waiting for stops list to load" (fun () ->
                        grappStopsSignal.WaitOne() |> ignore)

                    (scrapeGrapp args grappStopMap conn).Wait()
                ),
                // Every 10 minutes, fire immediately
                null, 0, 10*60*1000)

    if argFlagSet args "--szmapa" then
        measureTime "Waiting for stops list to load" (fun () ->
            grappStopsSignal.WaitOne() |> ignore)

        let conn = connGetter()
        let httpClient = new HttpClient()
        httpClient.Timeout <- TimeSpan.FromSeconds(15L)
        let scraper = SzMapa.TrainPositionScraper(grappStopMap)

        let timer = new PeriodicTimer(TimeSpan.FromSeconds(10L)) // Every 10 seconds
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
