// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.Gtfs

open System.IO
open System.Data.Common
open System.Runtime.InteropServices
open Npgsql

open JrUtil.Utils
open JrUtil.GtfsCsvSerializer
open JrUtil.GtfsModel
open JrUtil.GtfsModelMeta
open JrUtil.GtfsParser
open JrUtil.SqlRecordStore

let gtfsFeedToFolder () =
    // This is an attempt at speeding serialization up. In the end it didn't do
    // much, but this should be faster so I'm leaving it like this
    let agencySerializer = getRowsSerializer<Agency>
    let stopSerializer = getRowsSerializer<Stop>
    let routeSerializer = getRowsSerializer<Route>
    let tripSerializer = getRowsSerializer<Trip>
    let stopTimeSerializer = getRowsSerializer<StopTime>
    let calendarEntrySerializer = getRowsSerializer<CalendarEntry>
    let calendarExceptionSerializer = getRowsSerializer<CalendarException>
    let feedInfoSerializer = getRowsSerializer<FeedInfo>

    fun path feed ->
        Directory.CreateDirectory(path) |> ignore
        let serializeTo name ser obj =
            let filePath = Path.Combine(path, name)
            File.WriteAllText(filePath, ser obj)
        let serializeToOpt name ser obj =
            obj |> Option.iter (fun o -> serializeTo name ser o)
        serializeTo "agency.txt" agencySerializer feed.agencies
        serializeTo "stops.txt" stopSerializer feed.stops
        serializeTo "routes.txt" routeSerializer feed.routes
        serializeTo "trips.txt" tripSerializer feed.trips
        serializeTo "stop_times.txt" stopTimeSerializer feed.stopTimes
        serializeToOpt "calendar.txt" calendarEntrySerializer feed.calendar
        serializeToOpt "calendar_dates.txt"
                       calendarExceptionSerializer
                       feed.calendarExceptions
        match feed.feedInfo with
        | Some fi -> serializeTo "feed_info.txt" feedInfoSerializer [fi]
        | _ -> ()

let gtfsParseFolder () =
    // In Python, I'd make a dictionary of file name -> type
    // I think this is the best I can do without reflection.
    let fileParser name =
        let parser = getGtfsFileParser
        fun path -> parser (Path.Combine(path, name)) |> Seq.toArray
    let fileParserOpt name =
        let parser = getGtfsFileParser
        fun path ->
            let p = Path.Combine(path, name)
            if File.Exists(p) then Some <| (parser p |> Seq.toArray) else None

    let agenciesParser = fileParser "agency.txt"
    let stopsParser = fileParser "stops.txt"
    let routesParser = fileParser "routes.txt"
    let tripsParser = fileParser "trips.txt"
    let stopTimesParser = fileParser "stop_times.txt"
    let calendarParser = fileParserOpt "calendar.txt"
    let calendarExceptionsParser = fileParserOpt "calendar_dates.txt"
    let feedInfoParser = fileParserOpt "feedinfo.txt"

    fun path ->
        let feed: GtfsFeed = {
            agencies = agenciesParser path
            stops = stopsParser path
            routes = routesParser path
            trips = tripsParser path
            stopTimes = stopTimesParser path
            calendar = calendarParser path
            calendarExceptions = calendarExceptionsParser path
            feedInfo =
                feedInfoParser path
                |> Option.map (fun fi -> fi.[0])
        }
        feed

let sqlCreateGtfsTablesNoConstrs conn =
    let table recType name pkeyCols indexCols =
        createTableFor conn recType name
        let pkey =
            pkeyCols
            |> Seq.map (fun f -> sprintf "\"%s\"" f)
            |> String.concat ", "
        executeSql conn
                   (sprintf """ALTER TABLE "%s" ADD PRIMARY KEY (%s)"""
                            (sqlIdent name) (sqlIdent pkey)) []
        for ic in indexCols do
            executeSql conn (sprintf """CREATE INDEX ON "%s" ("%s")"""
                                     (sqlIdent name) (sqlIdent ic)) []

    table typeof<Agency> "agencies" ["id"] ["name"]
    table typeof<Stop> "stops" ["id"] ["name"]
    table typeof<Route> "routes" ["id"]
        ["agencyId"; "shortName"; "longName"]
    table typeof<Trip> "trips" ["id"] ["routeId"; "serviceId"]
    table typeof<StopTime> "stopTimes" ["tripId"; "stopSequence"] []
    table typeof<CalendarEntry> "calendar" ["id"] []
    table typeof<CalendarException> "calendarExceptions" ["id"; "date"] []

let sqlCreateGtfsConstrs conn =
    // No foreign key constraint for parentStation because it'd make
    // inserting into the table annoying
    // Also no constraint for serviceId, because one of two tables could
    // contain the referenced rows.
    executeSql conn """
        ALTER TABLE routes ADD FOREIGN KEY (agencyid) REFERENCES agencies (id);
        ALTER TABLE trips ADD FOREIGN KEY (routeid) REFERENCES routes (id);
        ALTER TABLE stoptimes ADD FOREIGN KEY (tripid) REFERENCES trips (id);
        ALTER TABLE stoptimes ADD FOREIGN KEY (stopid) REFERENCES stops (id);
    """ []

let sqlCreateGtfsTables conn =
    sqlCreateGtfsTablesNoConstrs conn
    sqlCreateGtfsConstrs conn

let sqlInsertGtfsFeed =
    // TODO: Use COPY
    let inserter t tableName =
        let i = createSqlInserter t
        fun (conn: DbConnection) -> Array.iter (fun x -> i tableName conn x)
    let optionInserter t tableName =
        let i = inserter t tableName
        fun conn -> Option.iter (i conn)

    let agenciesInserter = inserter typeof<Agency> "agencies"
    let stopsInserter = inserter typeof<Stop> "stops"
    let routesInserter = inserter typeof<Route> "routes"
    let tripsInserter = inserter typeof<Trip> "trips"
    let stopTimesInserter = inserter typeof<StopTime> "stopTimes"
    let calendarInserter = optionInserter typeof<CalendarEntry> "calendar"
    let calendarExceptionsInserter =
        optionInserter typeof<CalendarException> "calendarExceptions"

    fun (conn: DbConnection) feed ->
        agenciesInserter conn feed.agencies
        stopsInserter conn feed.stops
        routesInserter conn feed.routes
        tripsInserter conn feed.trips
        stopTimesInserter conn feed.stopTimes
        calendarInserter conn feed.calendar
        calendarExceptionsInserter conn feed.calendarExceptions

let saveGtfsSqlSchema (conn: NpgsqlConnection) (schema: string) outpath =
    Directory.CreateDirectory(outpath) |> ignore
    let writeHeader recType (file: StreamWriter) =
        let header = getHeader recType |> String.concat ","
        file.Write(header + "\n")

    let save recType filename query =
        let path = Path.Combine(outpath, filename)
        use file = new StreamWriter(path)
        writeHeader recType file
        use reader =
            conn.BeginTextExport(sprintf "COPY %s TO STDOUT WITH (FORMAT csv)"
                                         query)
        // TextReader doesn't have .CopyTo, so this will have to do
        let buf = Array.create 4096 '\000'
        let mutable hasData = true;
        while hasData do
            let len = reader.ReadBlock(buf, 0, buf.Length)
            file.Write(buf.[0..len - 1])
            hasData <- len = buf.Length

    let saveTable recType filename tableName =
        save recType filename (sprintf "%s.%s" schema tableName)

    saveTable typeof<Agency> "agency.txt" "agencies"
    saveTable typeof<Stop> "stops.txt" "stops"
    saveTable typeof<Route> "routes.txt" "routes"
    saveTable typeof<Trip> "trips.txt" "trips"
    saveTable typeof<StopTime> "stop_times.txt" "stoptimes"
    save typeof<CalendarEntry> "calendar.txt" (sprintf """(
            SELECT id,
                   CASE WHEN weekdayservice[0] THEN 1 ELSE 0 END,
                   CASE WHEN weekdayservice[1] THEN 1 ELSE 0 END,
                   CASE WHEN weekdayservice[2] THEN 1 ELSE 0 END,
                   CASE WHEN weekdayservice[3] THEN 1 ELSE 0 END,
                   CASE WHEN weekdayservice[4] THEN 1 ELSE 0 END,
                   CASE WHEN weekdayservice[5] THEN 1 ELSE 0 END,
                   CASE WHEN weekdayservice[6] THEN 1 ELSE 0 END,
                   to_char(startdate, 'YYYYMMDD'),
                   to_char(enddate, 'YYYYMMDD')
            FROM %s.calendar
        )""" schema)
    save typeof<CalendarException> "calendar_dates.txt" (sprintf """(
            SELECT id, to_char(date, 'YYYYMMDD'), exceptiontype
            FROM %s.calendarexceptions
        )""" schema)

let sqlLoadGtfsFeed conn path =
    // TODO: See if creating functions beforehand has a reasonable impact
    // Maybe consider global memoization for reflection?
    let inserter t table filename transform =
        let lines = fileLinesSeq (Path.Combine(path, filename))
        let transformToModel = gtfsTransformToModel t (lines |> Seq.head)
        let colsNullable = recordSqlColsNullable t

        lines
        |> Seq.tail
        |> Seq.map (splitLine >> transformToModel >> transform)
        |> Seq.chunkBySize 100000 // Some random number
        |> Seq.iter (sqlCopyInText conn table colsNullable)

    inserter typeof<Agency> "agencies" "agency.txt" id
    inserter typeof<Stop> "stops" "stops.txt" id
    inserter typeof<Route> "routes" "routes.txt" id
    inserter typeof<Trip> "trips" "trips.txt" id
    inserter typeof<StopTime> "stopTimes" "stop_times.txt" id
    inserter typeof<CalendarEntry> "calendar" "calendar.txt" (fun row ->
        [| row.[0];
           "{" + (row.[1..7]
                  |> String.concat ",") + "}";
           row.[8];
           row.[9] |])
    inserter typeof<CalendarException> "calendarExceptions" "calendar_dates.txt" id
