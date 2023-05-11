// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Jdf109Model

open NodaTime

open JrUtil
open JrUtil.CsvParser

type JdfVersion = {
    version: string
}

type Agency = {
    id: string // IČO
    taxId: string option // DIČ
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
    agencyId: string
    routeType: JdfModel.RouteType
    reserved1: string option
    licenceNum: string option
    licenceValidFrom: LocalDate option
    licenceValidTo: LocalDate option
    timetableValidFrom: LocalDate
    timetableValidTo: LocalDate option
}

type Trip = {
    routeId: string
    id: int64
    [<CsvSpread(10)>]
    attributes: int option array
}

type RouteStop = {
    routeId: string
    // The stop's ID within this route
    // I'm not sure what this is or how it's used
    // TBD probably by analysis of existing files
    routeStopId: int64 // TODO
    reserved1: string option
    stopId: int64
    [<CsvSpread(3)>]
    attributes: int option array
}

type TripStop = {
    routeId: string
    tripId: int64
    routeStopId: int64
    stopId: int64
    stopPostNum: string option
    [<CsvSpread(2)>]
    attributes: int option array
    kilometer: decimal option
    arrivalTime: JdfModel.TripStopTime option
    departureTime: JdfModel.TripStopTime option
}

type RouteInfo = {
    routeId: string
    id: int64
    text: string
}

type ServiceNote = {
    routeId: string
    tripId: int64
    id: int64
    designation: string // TODO: What is this?
    timeType: JdfModel.ServiceNoteType option
    dateFrom: LocalDate option
    dateTo: LocalDate option
    note: string option
}

type AgencyAlternation = {
    routeId: string
    tripId: int64
    agencyId: string
    [<CsvSpread(6)>]
    attributes: int option array
    timeType: string option
    reserved1: string option
    dateFrom: LocalDate option
    dateTo: LocalDate option
}

type AlternateRouteName = {
    routeId: string
    altRouteNum: int
    country: string
}

type ReservationOptions = {
    routeId: string
    tripId: int64
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
    serviceNotes: ServiceNote array
    agencyAlternations: AgencyAlternation array
    alternateRouteNames: AlternateRouteName array
    reservationOptions: ReservationOptions array
}
