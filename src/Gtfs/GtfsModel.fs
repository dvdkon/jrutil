// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

// This file contains records that model a variant of GTFS which contains
// most of the standard GTFS fields and numerous extensions
// Fields or enumeration variants that are introduced by non-standard
// extensions are marked with "Extension:" comments and documented here.

module JrUtil.GtfsModel

open System
open JrUtil.GtfsCsvSerializer

type Agency = {
    [<CsvFieldName("agency_id")>] id: string option
    [<CsvFieldName("agency_name")>] name: string
    [<CsvFieldName("agency_url")>] url: string
    [<CsvFieldName("agency_timezone")>] timezone: string
    [<CsvFieldName("agency_lang")>] lang: string option
    [<CsvFieldName("agency_phone")>] phone: string option
    [<CsvFieldName("agency_fare_url")>] fareUrl: string option
    [<CsvFieldName("agency_email")>] email: string
}

type LocationType =
    | Stop
    | Station
    | StationEntrance

type Stop = {
    [<CsvFieldName("stop_id")>] id: string
    [<CsvFieldName("stop_code")>] code: string option
    [<CsvFieldName("stop_name")>] name: string
    [<CsvFieldName("stop_desc")>] desc: string option
    [<CsvFieldName("stop_lat")>] lat: decimal
    [<CsvFieldName("stop_lon")>] lon: decimal
    [<CsvFieldName("zone_id")>] zoneId: string option
    [<CsvFieldName("stop_url")>] url: string option
    [<CsvFieldName("location_type")>] locationType: LocationType option
    [<CsvFieldName("parent_station")>] parentStation: string option
    [<CsvFieldName("stop_timezone")>] timezone: string option
    // I'd love to have this as an enum, but the semantics are
    // too complex for a simple one. Maybe with a custom parser
    // one day (it'd need to know the value of locationType)
    [<CsvFieldName("wheelchair_boarding")>] wheelchairBoarding: int option
}

type Route = {
    [<CsvFieldName("route_id")>] id: string
    [<CsvFieldName("agency_id")>] agencyId: string option
    [<CsvFieldName("route_short_name")>] shortName: string option
    [<CsvFieldName("route_long_name")>] longName: string option
    [<CsvFieldName("route_desc")>] desc: string option
    // TODO: Enum? It'd be pretty big
    [<CsvFieldName("route_type")>] routeType: string
    [<CsvFieldName("route_url")>] url: string option
    [<CsvFieldName("route_color")>] color: string option
    [<CsvFieldName("route_text_color")>] textColor: string option
    [<CsvFieldName("route_sort_order")>] sortOrder: int option
}

type BicycleCapacity =
    | OneOrMore
    | None

type Trip = {
    [<CsvFieldName("route_id")>] routeId: string
    [<CsvFieldName("service_id")>] serviceId: string
    [<CsvFieldName("trip_id")>] id: string
    [<CsvFieldName("trip_headsign")>] headsign: string option
    [<CsvFieldName("trip_short_name")>] shortName: string option
    [<CsvFieldName("direction_id")>] directionId: string option // "1" | "0"
    [<CsvFieldName("block_id")>] blockId: string option
    [<CsvFieldName("shape_id")>] shapeId: string option
    [<CsvFieldName("wheelchair_accessible")>]
        wheelchairAccessible: string option
    [<CsvFieldName("bikes_allowed")>] bkiesAllowed: BicycleCapacity option
}

type ServiceType =
    | RegularlyScheduled
    | NoService
    | PhoneBefore
    | CoordinationWithDriver

type TimePoint =
    | Exact
    | Approximate

type StopTime = {
    [<CsvFieldName("trip_id")>] tripId: string
    // These two fields use TimeSpan, because it represents them better than
    // plain DateTime. For example, handling trips that cross day
    // boundaries with DateTime would be awkward
    [<CsvFieldName("arrival_time")>] arrivalTime: TimeSpan
    [<CsvFieldName("departure_time")>] departureTime: TimeSpan
    [<CsvFieldName("stop_id")>] stopId: string
    [<CsvFieldName("stop_sequence")>] stopSequence: int
    [<CsvFieldName("stop_headsign")>] headsign: string option
    [<CsvFieldName("pickup_type")>] pickupType: ServiceType option
    [<CsvFieldName("drop_off_type")>] dropoffType: ServiceType option
    [<CsvFieldName("shape_dist_travelled")>] shapeDistTravelled: decimal option
    [<CsvFieldName("timepoint")>] timepoint: TimePoint option
}


type CalendarEntry = {
    [<CsvFieldName("service_id")>] id: string
    [<CsvSpread([|"monday"; "tuesday"; "wednesday"; "thursday";
                  "friday"; "saturday"; "sunday"|])>]
        weekdayService: bool array
    [<CsvFieldName("start_date")>] startDate: DateTime
    [<CsvFieldName("end_date")>] endDate: DateTime
}

type ExceptionType =
    | ServiceAdded
    | ServiceRemoved

type CalendarException = {
    [<CsvFieldName("service_id")>] id: string
    [<CsvFieldName("date")>] date: DateTime
    [<CsvFieldName("exception_type")>] exceptionType: ExceptionType
}

type FeedInfo = {
    [<CsvFieldName("feed_publisher_name")>] publisherName: string
    [<CsvFieldName("feed_publisher_url")>] publisherUrl: string
    [<CsvFieldName("feed_lang")>] lang: string
    [<CsvFieldName("feed_start_date")>] startDate: DateTime option
    [<CsvFieldName("feed_end_date")>] endDate: DateTime option
    [<CsvFieldName("feed_version")>] version: string option
}

// There are also other files, but I don't think JrUtil will be generating
// them right now

type GtfsFeed = {
    agencies: Agency array
    stops: Stop array
    routes: Route array
    trips: Trip array
    stopTimes: StopTime array
    calendar: CalendarEntry array
    calendarExceptions: CalendarException array
}
