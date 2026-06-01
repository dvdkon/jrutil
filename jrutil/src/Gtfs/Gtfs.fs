// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2025 David Koňařík

module JrUtil.Gtfs

open System.IO

open JrUtil.GtfsCsvSerializer
open JrUtil.GtfsModel
open JrUtil.GtfsParser

let gtfsFeedToFolder () =
    // This is an attempt at speeding serialization up. In the end it didn't do
    // much, but this should be faster so I'm leaving it like this
    let agencySerializer = getRowsSerializerWriter<Agency>
    let stopSerializer = getRowsSerializerWriter<Stop>
    let routeSerializer = getRowsSerializerWriter<Route>
    let tripSerializer = getRowsSerializerWriter<Trip>
    let stopTimeSerializer = getRowsSerializerWriter<StopTime>
    let calendarEntrySerializer = getRowsSerializerWriter<CalendarEntry>
    let calendarExceptionSerializer = getRowsSerializerWriter<CalendarException>
    let feedInfoSerializer = getRowsSerializerWriter<FeedInfo>

    fun path feed ->
        Directory.CreateDirectory(path) |> ignore
        let serializeTo name ser obj =
            let filePath = Path.Combine(path, name)
            use file = File.Open(filePath, FileMode.Create)
            ser file obj
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

/// The GTFS standard requires that some fields we have no way of filling from
/// source data be populated, and some GTFS-consuming software actually needs
/// them filled, even if the user doesn't. To make our exports GTFS-compliant,
/// fill them with nonsense.
let fillStandardRequiredFields (feed: GtfsFeed) =
    { feed with
        agencies =
            feed.agencies
            |> Array.map (fun a ->
                { a with
                    url = a.url
                        |> Option.defaultValue "jrutil://invalid"
                        |> Some
                })
        stops =
            feed.stops
            |> Array.map (fun s ->
                { s with
                    lat = s.lat |> Option.defaultValue 0m |> Some
                    lon = s.lon |> Option.defaultValue 0m |> Some
                })
    }

let deduplicateCalendar (feed: GtfsFeed) =
    let allCalEntries = feed.calendar |> Option.defaultValue [||]
    let allCalExcs = feed.calendarExceptions |> Option.defaultValue [||]
    let serviceIds =
        Set.union
            (allCalEntries |> Array.map (fun ce -> ce.id) |> Set)
            (allCalExcs |> Array.map (fun c -> c.id) |> Set)
    let calById = allCalEntries |> Array.map (fun ce -> ce.id, ce) |> Map
    let excsById = allCalExcs |> Array.groupBy (fun ce -> ce.id) |> Map
    let deduplicated =
        serviceIds
        |> Seq.map (fun si ->
            let calEntry = calById |> Map.tryFind si
            let calExcs = excsById |> Map.tryFind si |> Option.defaultValue [||]
            let dataWithoutId =
                calEntry |> Option.map (fun ce -> { ce with id = "" }),
                calExcs |> Array.map (fun ce -> { ce with id = "" })
                        |> Array.sort
            dataWithoutId, si
        )
        |> Seq.groupBy fst
        |> Seq.mapi (fun i (_, items) ->
            let items = items |> Seq.toArray
            let oldIds = items |> Array.map (fun (_, si) -> si)
            let (reprCalEntry, reprCalExcs), _ = items |> Array.head
            // Put the bitmap string in the ID to make manual debugging easier
            let bitmapStr =
                reprCalEntry
                |> Option.map (fun ce ->
                    ce.weekdayService
                    |> Array.map (fun b -> if b then '1' else '0')
                    |> System.String)
                |> Option.defaultValue "exc"
            let newId = $"CAL-{bitmapStr}-{i}"

            oldIds,
            newId,
            reprCalEntry |> Option.map (fun ce -> { ce with id = newId }),
            reprCalExcs |> Array.map (fun ce -> { ce with id = newId })
        )
        |> Seq.toArray
    let idMap =
        deduplicated
        |> Array.collect (fun (os, n, _, _) ->
            os |> Array.map (fun o -> o, n))
        |> Map
    { feed with
        trips =
            feed.trips
            |> Array.map (fun t -> { t with serviceId = idMap[t.serviceId] })
        calendar =
            Some <| (deduplicated |> Array.choose (fun (_, _, ce, _) -> ce))
        calendarExceptions =
            Some <| (deduplicated |> Array.collect (fun (_, _, _, ces) -> ces))
    }
