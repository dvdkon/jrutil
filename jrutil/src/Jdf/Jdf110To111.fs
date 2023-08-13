// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf110To111

// The F# compiler hates lists in parentheses
#nowarn "0058"

open NodaTime

open JrUtil
open JrUtil.AutoCopy
open JrUtil.Utils

let jdf110To111Converter () =
    let routeAC =
        getAutoCopier<Jdf110Model.Route, JdfModel.Route> (nameGetters [
            "oneWay", constant false >> box
            "timetableValidTo", (fun (r: Jdf110Model.Route) ->
                (match r.timetableValidTo with
                 | Some date -> box date
                 // This is just an arbitrary large date
                 | None -> LocalDate(3000, 1, 1) |> box))
        ])
    let tripStopAC =
        getAutoCopier<Jdf110Model.TripStop, JdfModel.TripStop> (nameGetters [
            "minArrivalTime", constant None >> box
            "maxDepartureTime", constant None >> box
        ])
    let transferAC =
        getAutoCopier<Jdf110Model.Transfer, JdfModel.Transfer> (nameGetters [
            "note", constant None >> box
        ])
    let batchAC =
        getAutoCopier<Jdf110Model.JdfBatch, JdfModel.JdfBatch>
            (nameGetters [
                "routes", (fun (b: Jdf110Model.JdfBatch) ->
                    Array.map routeAC b.routes |> box)
                "tripStops", (fun b -> Array.map tripStopAC b.tripStops |> box)
                "transfers", (fun b -> Array.map transferAC b.transfers |> box)
            ])
    batchAC
