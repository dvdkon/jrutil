// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2020 David Koňařík

module JrUtil.JdfModel

open System
open System.Globalization
open System.Text.RegularExpressions
open NodaTime
open NodaTime.Text

open JrUtil.Utils
open JrUtil.CsvMetadata
open JrUtil.UnionCodec

// A bit of trickery to deal with F#'s lack of support for mutually recursive
// type and let binding
let mutable attributeDefaultParser: (string -> obj) option = None

// This type is shared between all JDF versions, because, as far as I can tell,
// it's the only thing (this and RouteType, anyway) that sometimes "loses
// options" (for example, JDF 1.10 has "s" for "self-service trains", but 1.11
// omits it completely)
// This makes it sort of a mess by being a merger of multiple format versions.
// This also means that this library will accept "hybrid" JDF files
// (e.g. JDF 1.11 file with "s" attribute)
// At least some kind of order is kept by the version comments
type Attribute =
    // Taken from JDF 1.11
    | [<StrValue("X")>] WeekdayService
    | [<StrValue("+")>] HolidaySundayService
    | DayOfWeekService of int
    | [<StrValue("R")>] ReservationAvailable
    | [<StrValue("#")>] OnlyWithReservation
    | [<StrValue("|")>] NotStopping
    | [<StrValue("<")>] TakesDiversion
    | [<StrValue("@")>] WheelchairAccessible
    | [<StrValue("%")>] FoodAvailable
    | [<StrValue("W")>] ToiletsAvailable
    | [<StrValue("w")>] WheelchairAccessibleToilets
    | [<StrValue("x")>] RequestStop
    | [<StrValue("~")>] CityTransportTransfer
    | [<StrValue("(")>] ExitOnly
    | [<StrValue(")")>] BoardingOnly
    | [<StrValue("$")>] BorderStopOnly
    | [<StrValue("{")>] PartlyWheelchairAccessible
    | [<StrValue("}")>] VisuallyImpairedAccessible
    | [<StrValue("[")>] BaggageTransport
    | [<StrValue("O")>] BicycleTransport
    | [<StrValue("v")>] RailwayTransfer
    | [<StrValue("§")>] TravelExclusion0
    | [<StrValue("A")>] TravelExclusion1
    | [<StrValue("B")>] TravelExclusion2
    | [<StrValue("C")>] TravelExclusion3
    | [<StrValue("T")>] CommisionServiceOnly
    | [<StrValue("!")>] ConditionalService
    | [<StrValue("t")>] AccessibilityTerminal // TODO: What is this?
    | [<StrValue("b")>] LineTransportTransfer
    | [<StrValue("U")>] MetroTransfer // I'm tempted to name this UBahnTransfer
    | [<StrValue("S")>] ShipTransfer
    | [<StrValue("J")>] AirportNearby
    | [<StrValue("P")>] ParkAndRideNearby
    // Missing from JDF 1.11, taken from JDF 1.10
    | [<StrValue("I")>] PartOfIntegratedTransport
    | [<StrValue("s")>] SelfServiceTicketTrain

    with
    static member CsvParse(str: string) =
        if Regex.IsMatch(str, @"^\d+$")
        then DayOfWeekService (int str) |> box
        else match attributeDefaultParser with
             | Some dp -> dp str
             | None -> let dp = getUnionParser typeof<Attribute>
                       attributeDefaultParser <- Some (dp)
                       dp str
    static member private baseSerializer =
        getUnionSerializer typeof<Attribute>
    member this.CsvSerialize() =
        match this with
        | DayOfWeekService d -> string d
        | _ -> Attribute.baseSerializer this


// TODO: .NET reflection docs (not the F#-specific ones) say, that field order
// isn't guaranteed by reflection. Maybe these types' fields will need some
// attributes to guarantee their order

type JdfVersion = {
    version: string
    // Číslo dopravního úřadu
    duNum: int option
    region: string option
    batchId: string option
    creationDate: LocalDate option
    generator: String option
}

type Stop = {
    id: int64
    town: string
    district: string option
    nearbyPlace: string option // In practice used for the stop name
    // Two-letter abbreviation of Czech Republic okres + AB for Prague
    // Custom enum for CIS JŘ based on SPZ prefixes (see data/regions.json)
    regionId: string option
    country: string option
    [<CsvSpread(6)>]
    attributes: int option array
}

type StopPost = {
    stopId: int64
    stopPostId: int64
    name: string option
    description: string option
    postName: string option // Name within the station
    reserved1: string option
    reserved2: string option
}

type AttributeRef = {
    attributeId: int
    // It would be better to have a separate type for all the uses of
    // these attributes, but that would be too complicated with the way
    // they're stored
    value: Attribute
    reserved1: string option
}

type CompanyType =
    // XXX: "" and "0" are here to parse bad batches
    | [<StrValue("1")>] [<StrValue("0")>] [<StrValue("")>] Corporation
    | [<StrValue("2")>] NaturalPerson

type Agency = {
    id: string // IČO
    taxId: string option // DIČ
    name: string
    companyType: CompanyType
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

// This union doesn't quite fit JDF 1.8, but there's really no way to make it
// fit and the misatches are only minor.
type RouteType =
    | [<StrValue("A")>] City
    | [<StrValue("B")>] CityAndAdjacent
    | [<StrValue("M")>] International // From JDF 1.8
    | [<StrValue("N")>] InternationalNoNational
    | [<StrValue("P")>] InternationalOrNational
    | [<StrValue("V")>] Regional
    | [<StrValue("W")>] ExtraDistrict // From JDF 1.8
    | [<StrValue("Z")>] ExtraRegional // TODO: Better naming?
    | [<StrValue("D")>] LongDistanceNational

type TransportMode =
    | [<StrValue("A")>] Bus
    | [<StrValue("E")>] Tram
    | [<StrValue("L")>] CableCar
    | [<StrValue("M")>] Metro
    | [<StrValue("P")>] Ferry
    | [<StrValue("T")>] Trolleybus

// Using GTFS terminology here to have at least some consistency
type Route = {
    id: string
    name: string
    agencyId: string
    routeType: RouteType
    transportMode: TransportMode
    detour: bool
    grouped: bool // TODO
    usesStopPosts: bool
    oneWay: bool
    reserved1: string option
    licenceNum: string option
    licenceValidFrom: LocalDate option
    licenceValidTo: LocalDate option
    timetableValidFrom: LocalDate
    timetableValidTo: LocalDate
    agencyDistinction: int // forms a 2-field foreign key with agencyId
    idDistinction: int // forms a 2-field primary key with id
}

type RouteIntegration = {
    routeId: string
    entryNum: int // Incremented per routeId
    transportSystemId: int64
    routeName: string
    preferential: bool
    reserved1: string
    routeDistinction: int // forms a 2-field foreign key with routeId
}

type Trip = {
    routeId: string
    id: int64
    [<CsvSpread(10)>]
    attributes: int option array
    tripGroupId: int64 option
    routeDistinction: int // forms a 2-field foreign key with routeId
}

type TripGroup = {
    id: int64
    entryNum: int // Incremented per route
    name: string
    description: string option
    reserved1: string
}

type RouteStop = {
    routeId: string
    // The stop's ID within this route
    // Sequential, signifies place within printed timetable
    // (and therefore generally order of stop calls)
    routeStopId: int64 // TODO
    zone: string option
    stopId: int64
    approximateTime: int option // Minutes from route start
    [<CsvSpread(3)>]
    attributes: int option array
    routeDistinction: int
}

let private timePattern =
    LocalTimePattern.CreateWithInvariantCulture("HHmm")
type TripStopTime =
    // The annotations are here just because they look good
    // (and as documentation)
    | [<StrValue("|")>] Passing
    // This is kind of a silly sounding name
    // It means that the vehicle doesn't even pass the stop, because
    // it's routed differently
    | [<StrValue("<")>] NotPassing
    | StopTime of LocalTime

    with
    static member CsvParse(str) =
        match str with
        | "|" -> Passing
        | "<" -> NotPassing
        | _ -> timePattern.Parse(str).GetValueOrThrow() |> StopTime
    member this.CsvSerialize() =
        match this with
        | Passing -> "|"
        | NotPassing -> "<"
        | StopTime t -> timePattern.Format(t)

type TripStop = {
    routeId: string
    tripId: int64
    // For odd trips, the order they visit the stops is lowest-to-highest. For
    // even trips, it's in reverse.
    routeStopId: int64
    stopId: int64
    stopPostId: int64 option
    // Standard says this should be an int, but it's a string in practice
    stopPostNum: string option
    [<CsvSpread(3)>]
    attributes: int option array
    kilometer: decimal option
    arrivalTime: TripStopTime option
    departureTime: TripStopTime option
    minArrivalTime: TripStopTime option
    maxDepartureTime: TripStopTime option
    routeDistinction: int
}

type RouteInfo = {
    routeId: string
    id: int64
    text: string
    routeDistinction: int
}

type ServiceNoteType =
    | [<StrValue("1")>] Service
    | [<StrValue("2")>] ServiceAlso
    | [<StrValue("3")>] ServiceOnly
    | [<StrValue("4")>] NoService
    | [<StrValue("5")>] ServiceOddWeeks
    | [<StrValue("6")>] ServiceEvenWeeks
    | [<StrValue("7")>] ServiceOddWeeksFromTo
    | [<StrValue("8")>] ServiceEvenWeeksFromTo

type ServiceNote = {
    routeId: string
    tripId: int64
    id: int64
    designation: string
    noteType: ServiceNoteType option
    dateFrom: LocalDate option
    dateTo: LocalDate option
    note: string option
    routeDistinction: int
}

type Transfer = {
    transferType: string
    routeId: string
    tripId: int64
    routeStopId: int64
    transferRouteId: int64 option
    // All these IDs point to the global stop register, *not* to local stop IDs
    transferStopId: int64 option
    transferStopPostId: int64 option
    transferEndStopId: int64 option
    transferEndStopPostId: int64 option
    waitMinutes: int option
    note: string option
    routeDistinction: int
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
    agencyDistinction: int
    routeDistinction: int
}

type AlternateRouteName = {
    routeId: string
    altRouteNum: int
    country: string
    routeDistinction: int
}

type ReservationOptions = {
    routeId: string
    tripId: int64
    note: string
    routeDistinction: int
}

type GeodataPrecision =
    | [<StrValue("S")>] StopPrecise
    | [<StrValue("T")>] TownPrecise

// JrUtil extension
type StopLocation = {
    stopId: int64
    lat: decimal
    lon: decimal
    precision: GeodataPrecision
}

type JdfBatch = {
    version: JdfVersion
    stops: Stop array
    stopPosts: StopPost array
    agencies: Agency array
    routes: Route array
    routeIntegrations: RouteIntegration array
    routeStops: RouteStop array
    trips: Trip array
    tripGroups: TripGroup array
    tripStops: TripStop array
    routeInfo: RouteInfo array
    attributeRefs: AttributeRef array
    serviceNotes: ServiceNote array
    transfers: Transfer array
    agencyAlternations: AgencyAlternation array
    alternateRouteNames: AlternateRouteName array
    reservationOptions: ReservationOptions array
    stopLocations: StopLocation array
}
