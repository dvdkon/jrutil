// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf110Model

open JrUtil
open JrUtil.Utils
open JrUtil.CsvParser
open System

// This file is basically just parts of the JdfModel.fs file copypasted here
// with some changes to reflect JDF 1.10
// At one point, this file had the whole of JdfModel.fs with some minor
// modifications. This made it totally independent of the "current version",
// but amounted to too many lines of code, especially when "upgrading" batches.

type Route = {
    id: string
    name: string
    agencyId: string
    routeType: JdfModel.RouteType
    transportMode: JdfModel.TransportMode
    detour: bool
    grouped: bool
    usesStopPosts: bool
    reserved1: string option
    licenceNum: string option
    licenceValidFrom: Date option
    licenceValidTo: Date option
    timetableValidFrom: Date
    timetableValidTo: Date option
    agencyDistinction: int
    idDistinction: int
}

type TripStop = {
    routeId: string
    tripId: int64
    routeStopId: int64
    stopId: int64
    stopPostId: int64 option
    stopPostNum: int option
    [<CsvSpread(2)>]
    attributes: int option array
    kilometer: decimal option
    arrivalTime: JdfModel.TripStopTime option
    departureTime: JdfModel.TripStopTime option
    routeDistinction: int
}

type Transfer = {
    transferType: string
    routeId: string
    tripId: int64
    routeStopId: int64
    // All points to global register
    transferRouteId: int64 option
    transferStopId: int64 option
    transferStopPostId: int64 option
    transferEndStopId: int64 option
    transferEndStopPostId: int64 option
    waitMinutes: int option
    routeDistinction: int
}

type JdfBatch = {
    version: JdfModel.JdfVersion
    stops: JdfModel.Stop array
    stopPosts: JdfModel.StopPost array
    agencies: JdfModel.Agency array
    routes: Route array
    routeIntegrations: JdfModel.RouteIntegration array
    routeStops: JdfModel.RouteStop array
    trips: JdfModel.Trip array
    tripGroups: JdfModel.TripGroup array
    tripStops: TripStop array
    routeInfo: JdfModel.RouteInfo array
    attributeRefs: JdfModel.AttributeRef array
    routeTimes: JdfModel.RouteTime array
    transfers: Transfer array
    agencyAlternations: JdfModel.AgencyAlternation array
    alternateRouteNames: JdfModel.AlternateRouteName array
    reservationOptions: JdfModel.ReservationOptions array
}
