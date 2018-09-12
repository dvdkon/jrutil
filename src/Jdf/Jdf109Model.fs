// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf109Model

open JrUtil
open JrUtil.CsvParser
open System

type JdfVersion = {
    version: string
}

type Agency = {
    id: int // IČO
    taxId: int option // DIČ
    name: string
    companyType: JdfModel.CompanyType
    personName: string
    officeAddress: string
    officePhoneNum: string
    controlPhoneNum: string option
    infoPhoneNum: string option
    faxNum: string option
    email: string option
    website: string option
}

type Route = {
    id: string
    name: string
    agencyId: int
    routeType: JdfModel.RouteType
    reserved1: string option
    licenceNum: string option
    licenceValidFrom: DateTime option
    licenceValidTo: DateTime option
    timetableValidFrom: DateTime
    timetableValidTo: DateTime option
}

type Trip = {
    routeId: string
    id: int
    [<CsvSpread(10)>]
    attributes: int option array
}

type RouteStop = {
    routeId: string
    // The stop's ID within this route
    // I'm not sure what this is or how it's used
    // TBD probably by analysis of existing files
    routeStopId: int // TODO
    reserved1: string option
    stopId: int
    [<CsvSpread(3)>]
    attributes: int option array
}

type TripStop = {
    routeId: string
    tripId: int
    routeStopId: int
    stopId: int
    stopPostNum: int option
    [<CsvSpread(2)>]
    attributes: int option array
    kilometer: decimal option
    arrivalTime: JdfModel.TripStopTime option
    departureTime: JdfModel.TripStopTime option
}

type RouteInfo = {
    routeId: string
    id: int
    text: string
}

type RouteTime = {
    routeId: string
    tripId: int
    id: int
    designation: string // TODO: What is this?
    timeType: JdfModel.RouteTimeType option
    dateFrom: DateTime option
    dateTo: DateTime option
    note: string option
}

type AgencyAlternation = {
    routeId: string
    tripId: int
    agencyId: int
    [<CsvSpread(6)>]
    attributes: int option array
    timeType: string option
    reserved1: string option
    dateFrom: DateTime option
    dateTo: DateTime option
}

type AlternateRouteName = {
    routeId: string
    altRouteNum: int
    country: string
}

type ReservationOptions = {
    routeId: string
    tripId: int
    note: string
}

type JdfBatch = {
    version: JdfVersion
    stops: JdfModel.Stop array
    agencies: Agency array
    routes: Route array
    routeStops: RouteStop array
    trips: Trip array
    tripStops: TripStop array
    routeInfo: RouteInfo array
    attributeRefs: JdfModel.AttributeRef array
    routeTimes: RouteTime array
    agencyAlternations: AgencyAlternation array
    alternateRouteNames: AlternateRouteName array
    reservationOptions: ReservationOptions array
}
