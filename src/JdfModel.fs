module JrUtil.JdfModel

open System
open JrUtil.JdfCsvParser

// TODO: .NET reflection docs (not the F#-specific ones) say, that field order
// isn't guaranteed by reflection. Maybe these types' fields will need some
// attributes to guarantee their order

type JdfVersion = {
    version: string
    duNum: int option // TODO: What is this exactly?
    region: string option
    batchId: string option
    creationDate: DateTime option
    name: String option
}

type Stop = {
    id: int
    town: string
    district: string option
    nearbyPlace: string option // In practice used for the stop name
    nearbyTownId: string option
    country: string
    [<CsvSpread(6)>]
    attributes: string array
}

type StopPost = {
    stopId: int
    stopPostId: int
    name: string option
    description: string option
    postName: string option // Name within the station
    reserved1: string option
    reserved2: string option
}

type AttributeRef = {
    attributeId: int
    value: string
    reserved1: string option
}

type Agency = {
    id: int // IČO
    taxId: int option // DIČ
    name: string
    companyType: int // TODO: enum
    personName: string
    officeAddress: string
    officePhoneNum: string
    controlPhoneNum: string option
    infoPhoneNum: string option
    faxNum: string option
    email: string option
    website: string option
    idDistinction: int // forms a 2-field primary key with id
}

// Using GTFS terminology here to have at least some consistency
type Route = {
    id: int
    name: string
    agencyId: int
    routeType: string
    transportMode: string
    detour: bool
    grouped: bool // TODO
    usesStopPosts: bool
    oneWay: bool
    reserved1: string option
    licenceNum: string option
    licenceValidFrom: DateTime option
    licenceValidTo: DateTime option
    timetableValidFrom: DateTime
    timetableValidTo: DateTime
    agencyDistinction: int // forms a 2-field foreign key with agencyId
    idDistinction: int // forms a 2-field primary key with id
}

type RouteIntegration = {
    routeId: int
    entryNum: int // Incremented per routeId
    transportSystemId: int
    routeName: string
    preferential: bool
    reserved1: string
    routeDistinction: int // forms a 2-field foreign key with routeId
}

type Trip = {
    routeId: int
    id: int
    [<CsvSpread(10)>]
    attributes: string array
    tripGroupId: int option
    routeDistinction: int // forms a 2-field foreign key with routeId
}

type TripGroup = {
    id: int
    entryNum: int // Incremented per route
    name: string
    description: string option
    reserved1: string
}

type RouteStop = {
    routeId: int
    // The stop's ID within this route
    // I'm not sure what this is or how it's used
    // TBD probably by analysis of existing files
    routeStopId: int // TODO
    zone: string option
    stopId: int
    approximateTime: int option // Minutes from route start
    [<CsvSpread(3)>]
    attributes: string array
    routeDistinction: int
}

type TripStop = {
    routeId: int
    tripId: int
    routeStopId: int
    stopId: int
    stopPostId: int option
    stopPostNum: int option
    [<CsvSpread(3)>]
    attributes: string array
    kilometer: float option
    arrivalTime: string // TODO
    departureTime: string
    minArrivalTime: string option
    maxArrivalTime: string option
    routeDistinction: int
}

type RouteInfo = {
    routeId: int
    id: int
    text: string
    routeDistinction: int
}

type RouteTime = {
    routeId: int
    tripId: int
    id: int
    designation: string // TODO: What is this?
    timeType: string option
    dateFrom: DateTime option
    dateTo: DateTime option
    note: string option
    routeDistinction: int
}

type Transfer = {
    transferType: string
    routeId: int
    tripId: int
    routeStopId: int
    // All points to global register
    transferRouteId: int option
    transferStopId: int option
    transferStopPostId: int option
    transferEndStopId: int option
    transferEndStopPostId: int option
    waitMinutes: int option
    note: string option
    routeDistinction: int
}

type AgencyAlternation = {
    routeId: int
    tripId: int
    agencyId: int
    [<CsvSpread(6)>]
    attributes: string array
    timeType: string option
    reserved1: string option
    dateFrom: DateTime option
    dateTo: DateTime option
    agencyDistinction: int
    routeDistinction: int
}

type AlternativeRouteName = {
    routeId: int
    altRouteNum: int
    country: string
    routeDistinction: int
}

type ReservationOptions = {
    routeId: int
    tripId: int
    note: string
    routeDistinction: int
}
