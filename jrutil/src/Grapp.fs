// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2021 David Koňařík

module JrUtil.Grapp

open System
open System.IO
open System.Net
open System.Text.Json
open System.Text.RegularExpressions
open FSharp.Data
open NodaTime
open Serilog

open JrUtil.Utils
open JrUtil.RealTimeModel
open JrUtil.UndatedTimes

let baseUrl = "https://grapp.spravazeleznic.cz/"

type IndexData = {
    token: string
    cookies: CookieContainer
    carrierCodes: string array
    trainTypes: string array
    delayGroups: string array
}

type TrainDetail = {
    lastStopId: string option
    delay: float option
    routePageAvailable: bool
}

type GetTrainsWithFilterInput = {
    CarrierCode: string array
    PublicKindOfTrain: string array
    FreightKindOfTrain: string array
    KindOfExtraordinary: string array
    // Present in JS source code, but not sent
    // NoteOfExtraordinary: string array
    TrainRunning: bool
    PMD: bool
    TrainNoChange: int
    TrainOutOfOrder: bool
    Delay: string array
    DelayMin: int
    DelayMax: int
    SearchByTrainNumber: bool
    SearchByTrainName: bool
    SearchByTRID: bool
    SearchByVehicleNumber: bool
    SearchTextType: string
    SearchPhrase: string
    SelectedTrain: int
    RequestedBy: int
    OrderedBy: string
    UnRestriction: bool
    PlRestriction: bool
}

type GetTrainsWithFilterOutput =
    JsonProvider<const(__SOURCE_DIRECTORY__ + "/../samples/grapp_GetTrainsWithFilter.json")>

// To translate the station names GRAPP gives us we use the SR70 document. It's
// provided on an unstable URL as a .xls file, so the user is expected to
// transform this file into a CSV. Use the script `sr70_process.py` from
// `jrunify-ext-geodata` with param `--name=NÁZEV20`
type StopsCsv = ``CsvProvider,Schema="sr70(string), name(string), lat(float), lon(float)",HasHeaders="False"``

let readStopsCsv date filename =
    let csv = StopsCsv.Load(Path.GetFullPath(filename))
    csv.Rows |> Seq.map (fun r -> {
        id = sprintf "-SR70ST-%s" r.Sr70
        date = date
        name = r.Name
        lat = Some r.Lat
        lon = Some r.Lon
    })

let getNameIdMap (stops: Stop seq) =
    stops |> Seq.map (fun s -> s.name, s.id) |> Map

let stopIdForName (nameIdMap: Map<string, string>) name =
    match nameIdMap |> Map.tryFind name with
    | Some id -> id
    | None ->
        Log.Warning("No ID found for stop {Name}, using non-stable ID", name)
        "GRAPPST-" + name.Replace(" ", "_")

let fetchIndexData () =
    let cookies = CookieContainer()
    let resp = Http.RequestString(baseUrl, cookieContainer = cookies)
    let doc = HtmlDocument.Parse(resp)
    let cbValues selector =
        doc.CssSelect(selector)
        |> Seq.map (fun cb -> cb.AttributeValue("value"))
        |> Seq.toArray
    {
        token = doc.CssSelect("input#token").Head.AttributeValue("value")
        cookies = cookies
        carrierCodes = cbValues ".carrierCB"
        trainTypes = cbValues ".publicKindOfTrainCB"
        delayGroups = cbValues ".delayCB"
    }

let fetchAllTrainsSummary indexData () =
    let body = JsonSerializer.Serialize({
        CarrierCode = Array.concat [
            indexData.carrierCodes
            [| "f_o_r_e_i_g_n" |]
        ]
        PublicKindOfTrain = indexData.trainTypes
        FreightKindOfTrain = [||]
        KindOfExtraordinary = [||]
        TrainRunning = true
        PMD = false
        TrainNoChange = 0
        TrainOutOfOrder = true
        Delay = indexData.delayGroups
        DelayMax = -99999
        DelayMin = -99999
        SearchByTrainName = false
        SearchByTrainNumber = false
        SearchByTRID = false
        SearchByVehicleNumber = false
        SearchPhrase = ""
        SearchTextType = "0"
        SelectedTrain = -1
        RequestedBy = -1
        OrderedBy = ""
        UnRestriction = true
        PlRestriction = true
    })
    let respStr =
        Http.RequestString(
            sprintf "%s/post/trains/GetTrainsWithFilter/%s"
                    baseUrl indexData.token,
            httpMethod = "POST",
            headers = ["Content-Type", "application/json; charset=utf-8"],
            cookieContainer = indexData.cookies,
            body = TextRequest body)
    let resp = GetTrainsWithFilterOutput.Parse(respStr)
    if resp.Status <> "OK" then
        failwithf "Error status returned: %s" resp.Status
    resp.Trains

/// XXX: Workaround for FSharp.Data HTML Parser bug:
/// https://github.com/fsharp/FSharp.Data/issues/1330
let fsharpDataHtmlWorkaround html =
    Regex("(&[^;]+;) +(&[^;]+;)").Replace(html, "$1&#32;$2")

// Returns Option to handle the case when the ID is invalid/no longer available
let fetchTrainDetail indexData nameIdMap trainId () =
    let resp =
        Http.RequestString(sprintf "%s/OneTrain/MainInfo/%s?trainId=%d"
                                   baseUrl indexData.token trainId,
                           cookieContainer = indexData.cookies)
    // XXX: Remove the <html> tag when the FSharp.Data fix makes it into a
    // release
    let doc = HtmlDocument.Parse("<html>" + (fsharpDataHtmlWorkaround resp) + "<html>")
    if doc.CssSelect(".content") |> Seq.isEmpty then None else
        let tableMap =
            doc.CssSelect(".content .row")
            |> Seq.choose (fun row ->
                match row.Elements() with
                | [kc; vc] -> Some (kc.InnerText().Trim(),
                                    vc.InnerText().Trim())
                | _ -> None)
            |> Map
        let lastStop =
            Map.tryFind "potvrzená stanice" tableMap
            |> Option.orElse (Map.tryFind "poslední známá poloha:" tableMap)
            |> Option.map (stopIdForName nameIdMap)
        let delay =
            Map.tryFind "náskok" tableMap
            |> Option.map ((+) "-")
            |> Option.orElse (Map.tryFind "zpoždění" tableMap)
            |> Option.map (fun ds ->
                if ds = "-" then 0.0
                else float <| ds.Replace("min", ""))
        Some {
            lastStopId = lastStop
            delay = delay
            routePageAvailable = doc.CssSelect(".action") |> Seq.isEmpty |> not
        }

let fetchTrainRoute indexData nameIdMap trainId () =
    let resp =
        Http.RequestString(sprintf "%s/OneTrain/RouteInfo/%s?trainId=%d"
                                   baseUrl indexData.token trainId,
                           cookieContainer = indexData.cookies)
    let doc = HtmlDocument.Parse(fsharpDataHtmlWorkaround resp)

    let currentStationIdx =
        match doc.CssSelect(".route .row")
              |> Seq.tryFindIndex (fun row ->
                  row.CssSelect("#currentStation") |> List.isEmpty |> not) with
        | Some idx -> idx
        | None -> failwith "Failed to get current station"

    // The route page only has times and not dates, so we need to find out how
    // many days ago the train departed and then go through the data and add a
    // day to all following times after midnight is crossed.
    // This has to be done separately for the "real-time" times and the
    // planned/"timetable" times
    // Sometimes this gives bad dates, for example international trains don't
    // show their full schedule, but still show a "last stop" on this page.
    // These should ideally be filtered out beforehand and never have this
    // function called on them.
    let undated =
        doc.CssSelect(".route .row")
        |> Seq.mapi (fun i row ->
            let stopped = row.Elements().[0].HasClass("bold")
            let cols =
                row.Elements()
                |> Seq.map (fun c -> c.InnerText().Trim())
                |> Seq.toArray
            let parseTime strTime =
                if strTime = "" then
                    None
                else
                    Some <| parseTime "H:mm" strTime
            let parseParTime strTime =
                if strTime = "" then
                    None
                else
                    if strTime.[0] <> '('
                       || strTime.[(strTime.Length - 1)] <> ')' then
                        failwithf "Time \"%s\" was not enclosed in ()" strTime
                    parseTime strTime.[1..(strTime.Length - 2)]
            {
                UndatedStopHistoryItem.stopId = stopIdForName nameIdMap cols.[0]
                // Don't save the projected times shown after the current
                // station
                arrivedAt =
                    if i > currentStationIdx then None else parseTime cols.[4]
                shouldArriveAt = parseParTime cols.[5]
                departedAt =
                    if i > currentStationIdx then None else parseTime cols.[8]
                shouldDepartAt = parseParTime cols.[9]
                stopped = Some stopped
            }
        )
        |> Seq.toArray

    let timezone = DateTimeZoneProviders.Tzdb.["Europe/Prague"]
    let now =
        SystemClock.Instance.GetCurrentInstant()
         .InZone(timezone)
         .LocalDateTime

    // XXX: Sometimes there are stops that later disappear. No important ones,
    // but it's curious nonetheless

    dateUndatedStopHistory timezone currentStationIdx now undated
    |> Seq.toArray

let fetchAllTrains indexData nameIdMap () =
    let trains = fetchAllTrainsSummary indexData ()
    trains |> Seq.choose (fun train ->
        let trainName = train.Title.Trim()
        try
            fetchTrainDetail indexData nameIdMap train.Id ()
            |> Option.bind (fun detail ->
                // We can't get the start day for a train without the full
                // route, so just exclude them not to pollute the resultant data
                if not detail.routePageAvailable then
                    Log.Debug("Route page not available for {Train}", trainName)
                    None
                else
                    let route =
                        fetchTrainRoute indexData nameIdMap train.Id ()
                    let startTime =
                        route.[0].shouldDepartAt
                        |> Option.orElse route.[0].departedAt
                        |> Option.get
                    let trainId = String.Join("-", trainName.Split().[..1])
                    Some {
                        tripId = sprintf "-CZTRAINT-%s" trainId
                        tripStartDate = startTime.Date
                        observationTime =
                            SystemClock.Instance.GetCurrentInstant()
                        coords = Some {
                            lat = float train.Gps.[0]
                            lon = float train.Gps.[1]
                        }
                        lastStopId = detail.lastStopId
                        delay = detail.delay
                        stopHistory = Some route
                        routeId = sprintf "-CZTRAINR-%s" trainId
                        shortName = Some trainName
                        routeShortName = Some trainName
                    })
        with e ->
            Log.Error(
                e, "Exception while getting Grapp position for {TrainID}/{TrainName}",
                train.Id, train.Title)
            None
    )
