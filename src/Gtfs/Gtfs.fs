// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Gtfs

open System.IO.Compression

open JrUtil.GtfsCsvSerializer
open JrUtil.GtfsModel
open System.IO
open System.IO

let gtfsFeedToFolder path (feed: GtfsFeed) =
    Directory.CreateDirectory(path) |> ignore
    let serializeTo name obj =
        let filePath = Path.Combine(path, name)
        File.WriteAllText(filePath, serializeRows obj)
    let serializeToOpt name obj =
        obj |> Option.iter (fun o -> serializeTo name o)
    serializeTo "agency.txt" feed.agencies
    serializeTo "stops.txt" feed.stops
    serializeTo "routes.txt" feed.routes
    serializeTo "trips.txt" feed.trips
    serializeTo "stop_times.txt" feed.stopTimes
    serializeToOpt "calendar.txt" feed.calendar
    serializeToOpt "calendar_dates.txt" feed.calendarExceptions
    match feed.feedInfo with
    | Some fi -> serializeTo "feed_info.txt" [fi]
    | _ -> ()
