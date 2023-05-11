// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf109To110
open System.Reflection
open System

// The F# compiler hates lists in parentheses
#nowarn "0058"

open JrUtil
open JrUtil.AutoCopy
open JrUtil.Utils
open JrUtil.ReflectionUtils

// A version of AutoCopy's "nameGetters" that creates a getter returning None
// if no name-based getter is available and the target type is an option
let nameOptGetters<'f> (gs: (string * ('f -> obj)) list) =
    let gsMap = Map gs
    fun (f: PropertyInfo) ->
        Map.tryFind f.Name gsMap
        |> Option.orElse (
            let t = f.PropertyType
            let srcType = typeof<'f>
            if srcType.GetMember(f.Name).Length = 0 &&
                typeIsOption t
            then Some (constant None >> box)
            else None)

let jdf109To110Converter () =
    let versionAC = getAutoCopier<Jdf109Model.JdfVersion, JdfModel.JdfVersion>
                        (nameOptGetters [])
    let agencyAC = getAutoCopier<Jdf109Model.Agency, JdfModel.Agency> (
        nameOptGetters [ "idDistinction", constant 0 >> box ])
    let routeAC = getAutoCopier<Jdf109Model.Route, Jdf110Model.Route> (
        nameOptGetters [
            "agencyDistinction", constant 0 >> box
            "idDistinction", constant 0 >> box
            "transportMode", constant JdfModel.Bus >> box
            "detour", constant false >> box
            "grouped", constant false >> box
            "usesStopPosts", constant false >> box
        ]
    )
    let idDistGG () =
        nameOptGetters [
            "routeDistinction", constant 0 >> box
            "agencyDistinction", constant 0 >> box
        ]
    let routeStopAC = getAutoCopier<Jdf109Model.RouteStop, JdfModel.RouteStop>
                          (idDistGG())
    let tripAC = getAutoCopier<Jdf109Model.Trip, JdfModel.Trip>
                     (idDistGG())
    let tripStopAC = getAutoCopier<Jdf109Model.TripStop, Jdf110Model.TripStop>
                         (idDistGG())
    let routeInfoAC = getAutoCopier<Jdf109Model.RouteInfo, JdfModel.RouteInfo>
                          (idDistGG())
    let serviceNoteAC = getAutoCopier<Jdf109Model.ServiceNote, JdfModel.ServiceNote>
                          (idDistGG())
    let agencyAltAC = getAutoCopier<Jdf109Model.AgencyAlternation,
                                    JdfModel.AgencyAlternation>
                          (idDistGG())
    let altRouteNameAC = getAutoCopier<Jdf109Model.AlternateRouteName,
                                       JdfModel.AlternateRouteName>
                             (idDistGG())
    let resOptAC = getAutoCopier<Jdf109Model.ReservationOptions,
                                 JdfModel.ReservationOptions>
                       (idDistGG())

    let nogg = nameOptGetters<Jdf109Model.JdfBatch> [
        "version", (fun b -> versionAC b.version |> box)
        "agencies", (fun b -> Array.map agencyAC b.agencies |> box)
        "routes", (fun b -> Array.map routeAC b.routes |> box)
        "routeStops", (fun b ->
            Array.map routeStopAC b.routeStops |> box)
        "trips", (fun b -> Array.map tripAC b.trips |> box)
        "tripStops", (fun b -> Array.map tripStopAC b.tripStops |> box)
        "routeInfo", (fun b ->
            Array.map routeInfoAC b.routeInfo |> box)
        "serviceNotes", (fun b ->
            Array.map serviceNoteAC b.serviceNotes |> box)
        "agencyAlternations", (fun b ->
            Array.map agencyAltAC b.agencyAlternations |> box)
        "alternateRouteNames", (fun b ->
            Array.map altRouteNameAC b.alternateRouteNames |> box)
        "reservationOptions", (fun b ->
            Array.map resOptAC b.reservationOptions |> box)
    ]
    let batchAC =
        getAutoCopier<Jdf109Model.JdfBatch, Jdf110Model.JdfBatch> (fun f ->
            match nogg f with
            | Some g -> Some g
            | None ->
                let t = f.PropertyType
                let srcType = typeof<Jdf109Model.JdfBatch>
                if srcType.GetMember(f.Name).Length = 0 && t.IsArray
                then
                    Some (fun o ->
                    Array.CreateInstance(t.GetElementType(), 0) |> box)
                else None
        )
    batchAC
