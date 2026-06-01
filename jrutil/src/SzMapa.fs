// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2024 David Koňařík

module JrUtil.SzMapa

open System
open System.Net.Http
open System.Text.Json
open System.Text.Json.Serialization
open NodaTime
open NodaTime.Text
open NodaTime.Serialization.SystemTextJson
open Serilog
open Serilog.Context

open JrUtil.RealTimeSql
open JrUtil.Utils

let endpointUrl = "https://mapy.spravazeleznic.cz/serverside/request2.php?module=Layers\OsVlaky&&action=load"

// A model of the response JSON. Unneeded fields are omitted.
type ResponseTrainProperties = {
    id: string
    [<JsonPropertyName("type")>] typ: string
    [<JsonPropertyName("tt")>] trainTypeAbbr: string
    [<JsonPropertyName("tn")>] trainNumber: string
    [<JsonPropertyName("na")>] trainName: string
    [<JsonPropertyName("fn")>] startStop: string
    [<JsonPropertyName("ln")>] destinationStop: string
    [<JsonPropertyName("cna")>] lastPoint: string
    [<JsonPropertyName("de")>] currentDelay: int
    [<JsonPropertyName("nna")>] nextPoint: string
    [<JsonPropertyName("r")>] companyId: string
    [<JsonPropertyName("rr")>] stayingInLastPoint: int
    [<JsonPropertyName("d")>] companyName: string
    [<JsonPropertyName("s")>] isBusReplacement: int
    [<JsonPropertyName("di")>] isDiverted: int
    [<JsonPropertyName("cp")>] lastPointTimetabledTime: LocalTime option
    [<JsonPropertyName("cr")>] lastPointMeasuredTime: LocalTime option
    [<JsonPropertyName("pde")>] predictedDelay: string
    [<JsonPropertyName("nsn")>] nextStop: string
    [<JsonPropertyName("nsn70")>] nextStopSr70: string
    [<JsonPropertyName("nst")>] nextStopTimetabledTime: LocalTime option
    [<JsonPropertyName("nsp")>] nextStopPredictedTime: LocalTime option
    [<JsonPropertyName("zst_sr70")>] nextPointSr70: string
}

type ResponseTrain = {
    properties: ResponseTrainProperties
}

type Response = {
    [<JsonPropertyName("md")>] timestamp: LocalDateTime
    result: ResponseTrain array
}

type TimeOrEmptyConverter() =
    inherit JsonConverter<LocalTime option>()
    let pattern = LocalTimePattern.CreateWithInvariantCulture("HH:mm")
    override this.Read(
            reader: byref<Utf8JsonReader>,
            typeToConvert: System.Type,
            options: JsonSerializerOptions) =
        let s = reader.GetString()
        if s = "" then None
        else Some <| pattern.Parse(s).Value

    override this.Write(
            writer: Utf8JsonWriter,
            value: LocalTime option,
            options: JsonSerializerOptions) =
        failwith "Unimplemented"

let jsonOpts =
    let o = JsonSerializerOptions()
    o.Converters.Add(TimeOrEmptyConverter())
    o.Converters.Add(
        NodaPatternConverter(
            LocalDateTimePattern.CreateWithInvariantCulture(
                "dd.MM.yyyy HH:mm:ss")))
    o


let fetchTrains (httpClient: HttpClient) () =
    task {
        let! resp = httpClient.GetAsync(endpointUrl)
        let respBody = resp.Content.ReadAsStream()
        return! JsonSerializer.DeserializeAsync<Response>(respBody, jsonOpts)
    }

type TrainPositionScraper(stopNameIdMap: Map<string, string>) =
    let mutable lastData = Map<string, ResponseTrainProperties> []
    let mutable lastTimestamp: option<LocalDateTime> = None
    // This needs to be mutable, because long-running scraping processes need
    // to be able to refresh stop data
    let mutable stopNameIdMap = stopNameIdMap

    let tripStartDatePattern =
        LocalDatePattern.CreateWithInvariantCulture("yyyyMMdd")
    let timezone = DateTimeZoneProviders.Bcl["Europe/Prague"]

    let stopIdForName name =
        match stopNameIdMap |> Map.tryFind name with
        | Some id -> id
        | None ->
            Log.Warning("No ID found for stop {Name}, using non-stable ID", name)
            "SZMAPAST-" + name.Replace(" ", "_")

    let tripStopIndexForTime (time: LocalTime) = time.Minute + time.Hour * 100
    // We only get bare times, so this function "recovers" the date by
    // assuming the times aren't too far from the current.
    // "date" here is a verb (ha!)
    let dateTime (timestamp: LocalDateTime) (t: LocalTime) =
        if t > (timestamp + Period.FromHours(12)).TimeOfDay then
            // The time is certainly in the previous day
            timestamp.Date + t - Period.FromDays(1)
        else
            timestamp.Date + t

    member this.setStopNameIdMap snim = stopNameIdMap <- stopNameIdMap

    member this.processTrain (timestamp: LocalDateTime)
                             (props: ResponseTrainProperties) =
        // TODO: Unify this ID format with one used in CZPTT converter
        let idSplit = props.id.Split("/")
        let tripId = sprintf "-CZTRAINT-%s:%s:%s:%s"
                             idSplit[4] idSplit[1] idSplit[2] idSplit[3]
        let tripStartDate =
            tripStartDatePattern.Parse(idSplit[5]).Value


        let shortName =
            [props.trainTypeAbbr; props.trainNumber; props.trainName]
            |> String.concat " "
        let tripDetail = {
            tripId = tripId
            tripStartDate = tripStartDate
            routeId = tripId
            shortName = Some shortName
            routeShortName = Some shortName
        }

        let lptt = props.lastPointTimetabledTime
                   |> Option.map (dateTime timestamp)
        let lpmt = props.lastPointMeasuredTime
                   |> Option.map (dateTime timestamp)
        let lpId = stopIdForName props.lastPoint
        match lptt, lpmt with
        | Some lptt, Some lpmt ->
            match lastData |> Map.tryFind props.id with
            | _ when props.stayingInLastPoint = 1 ->
                [| {
                    tripId = tripId
                    tripStartDate = tripStartDate
                    stopId = lpId
                    // We don't get any sequential ID, nor do we know the
                    // full list of stops. Fake it by using timetabled
                    // time.
                    tripStopIndex = tripStopIndexForTime lptt.TimeOfDay
                    timeZone = timezone
                    arrivedAt = Some lpmt
                    shouldArriveAt = Some lptt
                    departedAt = None
                    shouldDepartAt = None
                    // TODO: We only want stops for (dis)embarking here.
                    // Does this match?
                    stopped = Some true
                } |], [|tripDetail|]
            | Some oldProps when oldProps.lastPoint = props.lastPoint
                              && oldProps.stayingInLastPoint = 1 ->
                // If we already entered this stop in for arrival, we
                // need to upsert that same entry for departure
                let arrLpmt =
                    oldProps.lastPointMeasuredTime
                    |> Option.map (dateTime timestamp)
                    |> Option.defaultWith (fun _ ->
                        failwith "Measured arrival time missing \
                                  for departed stop")
                let arrLptt =
                    oldProps.lastPointTimetabledTime
                    |> Option.map (dateTime timestamp)
                    |> Option.defaultWith (fun _ ->
                        failwith "Timetabled arrival time missing \
                                  for departed stop")
                [| {
                    tripId = tripId
                    tripStartDate = tripStartDate
                    stopId = lpId
                    tripStopIndex = tripStopIndexForTime arrLptt.TimeOfDay
                    timeZone = timezone
                    arrivedAt = Some arrLpmt
                    shouldArriveAt = Some arrLptt
                    departedAt = Some lpmt
                    shouldDepartAt = Some lptt
                    stopped = Some true
                } |], [|tripDetail|]
            | _ ->
                // We don't know the arrival time.
                // XXX: If we stopped and started during a train's stay in some
                // station, we could potentially put in the same stop twice.
                // This seems unavoidable without doing something like first
                // reading the database. Hopefully it won't be a problem?
                [| {
                    tripId = tripId
                    tripStartDate = tripStartDate
                    stopId = lpId
                    tripStopIndex = tripStopIndexForTime lptt.TimeOfDay
                    timeZone = timezone
                    arrivedAt = None
                    shouldArriveAt = None
                    departedAt = Some lpmt
                    shouldDepartAt = Some lptt
                    stopped = Some false
                } |], [|tripDetail|]
        | _ -> failwith "Times for last point missing"

    member this.processResponse (resp: Response) =
        let newRows =
            resp.result
            |> Seq.map (fun train ->
                let props = train.properties
                let oldProps = lastData |> Map.tryFind props.id
                if oldProps = Some props then [||], [||] else
                    use _logCtx = LogContext.PushProperty("TrainId", props.id)
                    try
                        this.processTrain resp.timestamp props
                    with e ->
                        Log.Error(e, "Error processing")
                        [||], [||])
            |> concatTo2

        lastData <-
            resp.result
            |> Seq.map (fun t -> t.properties.id, t.properties)
            |> Map
        lastTimestamp <- Some resp.timestamp

        newRows
