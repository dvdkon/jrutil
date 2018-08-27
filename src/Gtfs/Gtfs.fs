// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Gtfs

open System.IO

open JrUtil.GtfsCsvSerializer
open JrUtil.GtfsModel
open JrUtil.GtfsParser


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
        fun path -> parser (Path.Combine(path, name))
    let fileParserOpt name =
        let parser = getGtfsFileParser
        fun path ->
            let p = Path.Combine(path, name)
            if File.Exists(p) then Some <| parser p else None

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
