// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CzPtt

open System
open System.IO
open System.Collections
open System.IO.Compression
open System.Xml.Serialization
open NodaTime

open JrUtil.Utils
open JrUtil.UnionCodec

exception CzPttInvalidException of msg: string
with override this.Message = this.msg

type CzpttMessage =
    | Timetable of CzPttXml.CzpttcisMessage
    | Cancellation of CzPttXml.CzCanceledPttMessage

type TrainType =
    // The StrValues actually reflect the generated enum, not the actual string
    // in XML
    | [<StrValue("Item0")>] PassengerPrivate
    | [<StrValue("Item1")>] PassengerPublic
    | [<StrValue("Item2")>] Freight

type TrainActivity =
    | [<StrValue("0001")>] Stops
    | [<StrValue("0026")>] CustomsStop
    | [<StrValue("0027")>] OtherStop
    | [<StrValue("0028")>] EmbarkOnly
    | [<StrValue("0029")>] DisembarkOnly
    | [<StrValue("0030")>] RequestStop
    | [<StrValue("0031")>] DepartsAtArrivalTime
    | [<StrValue("0032")>] DepartsAfterDisembark
    | [<StrValue("0033")>] NoConnectionWait
    | [<StrValue("0035")>] Preheating
    | [<StrValue("0040")>] PassWithoutStop
    // XXX: Not yet in data
    | [<StrValue("0043")>] JoinedTrains
    | [<StrValue("0044")>] Connection
    | [<StrValue("CZ01")>] StopsAfterStationOpened
    | [<StrValue("CZ02")>] WaitsLessThanHalfMinute
    // TODO: Will these ever be in static data?
    | [<StrValue("CZ03")>] HandicappedEmbark
    | [<StrValue("CZ04")>] HandicappedDisembark
    | [<StrValue("CZ05")>] WaitForDelayed
    | [<StrValue("0002")>] InternalStop
    | [<StrValue("CZ13")>] UnpublishedStop

// This is more complicated than necessary, because readers can't seek and
// XmlSerializer doesn't want to deserialize both generated types at the same
// time.
let parseFromReader (withReader: (TextReader -> obj) -> obj) =
    let cisSerializer = XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
    let cancelSerializer = XmlSerializer(typeof<CzPttXml.CzCanceledPttMessage>)
    try
        Timetable <| (withReader cisSerializer.Deserialize :?> _)
    with e1 ->
        try
            Cancellation <| (withReader cancelSerializer.Deserialize :?> _)
        with e2 ->
            failwithf "Failed deserializing CZPTT XML message:\n\
                       As CZPTTCISMessage: %A\nAs CZCanceledMessage: %A" e1 e2

let parseText (text: string) =
    parseFromReader (fun f ->
        use reader = new StringReader(text)
        f reader)

let parseFile (path: string) =
    use stream = File.OpenRead(path)
    parseFromReader (fun f ->
        stream.Seek(0, SeekOrigin.Begin) |> ignore
        use reader = new StreamReader(stream)
        f reader)

/// Takes a path and finds all .xml documents under that path (incl. in ZIP
/// files) and parses them
let rec parseAll (path: string) =
    if path.ToLower().EndsWith(".xml.zip")
       || path.ToLower().EndsWith(".xml.gz") then
        // GZip is not ZIP, but tell that to SŽ
        // The data's good, so I don't complain
        use stream = File.Open(path, FileMode.Open)
        use gzStream = new GZipStream(stream, CompressionMode.Decompress)
        use reader = new StreamReader(gzStream)
        let doc =  parseText <| reader.ReadToEnd()
        seq { path, doc }
    else if Path.GetExtension(path).ToLower() = ".xml" then
        seq { path, parseFile path }
    else if Path.GetExtension(path).ToLower() = ".zip" then
        ZipFile.OpenRead(path).Entries
        |> Seq.filter (fun entry ->
            Path.GetExtension(entry.Name).ToLower() = ".xml")
        |> Seq.map (fun entry ->
            use reader = new StreamReader(entry.Open())
            $"path//{entry.Name}", parseText <| reader.ReadToEnd())
    else if Directory.Exists(path) then
        Seq.concat [Directory.EnumerateFiles(path);
                    Directory.EnumerateDirectories(path)]
        |> Seq.collect parseAll
    else Seq.empty

let parseCalendar (calendar: CzPttXml.PlannedCalendar) =
    let startDate = LocalDate.FromDateTime(calendar.ValidityPeriod.StartDateTime)
    let endDate = calendar.ValidityPeriod.EndDateTime
                  |> nullableOpt
                  |> Option.map LocalDate.FromDateTime
                  |> Option.defaultValue startDate
    DateBitmap(
        DateInterval(startDate, endDate),
        calendar.BitmapDays
        |> Seq.map (fun c -> c = '1')
        |> Seq.toArray
        |> BitArray)

let serializeCalendar (bitmap: DateBitmap) =
    CzPttXml.PlannedCalendar(
        ValidityPeriod = CzPttXml.ValidityPeriod(
        StartDateTime = bitmap.Interval.Start.ToDateTimeUnspecified(),
        EndDateTime = bitmap.Interval.End.ToDateTimeUnspecified()),
        BitmapDays = String([|
            for i in 0 .. (bitmap.Bits.Length - 1) ->
                if bitmap.Bits.[i] then '1' else '0' |]))

let timetableIdentifier (czptt: CzPttXml.CzpttcisMessage) idt =
    czptt.Identifiers |> Array.find (fun ti -> ti.ObjectType = idt)

let identifierStr (ident: CzPttXml.TransportIdentifier) =
    sprintf "%s:%s:%s" ident.TimetableYear ident.Core ident.Variant

let locationActivities (loc: CzPttXml.CzpttLocation) =
    loc.TrainActivity
    |> Seq.map (fun ta -> ta.TrainActivityType |> parseUnion<TrainActivity>)

let locationTrainType (loc: CzPttXml.CzpttLocation) =
    loc.TrainType
    |> nullableOpt
    |> Option.map (fun tt -> tt.ToString() |> parseUnion<TrainType>)

let isPublicLocation (loc: CzPttXml.CzpttLocation) =
    (loc.TrainActivity |> nullOpt |> Option.defaultValue [||]).Length > 0
    && not (locationActivities loc |> Seq.contains InternalStop)
    && not (locationActivities loc |> Seq.contains UnpublishedStop)
    && locationTrainType loc = Some PassengerPublic
