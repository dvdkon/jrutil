// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2020 David Koňařík

module JrUtil.RealTimeModel

open NodaTime

type Coords = {
    lat: float
    lon: float
}

type StopHistoryItem = {
    stopId: string
    // A per-trip counter for stops
    tripStopIndex: int
    timeZone: DateTimeZone
    arrivedAt: LocalDateTime option
    shouldArriveAt: LocalDateTime option
    departedAt: LocalDateTime option
    shouldDepartAt: LocalDateTime option
}

/// Represents a trip's current position as observed at one moment
type TripPosition = {
    tripId: string
    tripStartDate: LocalDate
    observationTime: Instant
    coords: Coords option
    lastStopId: string option
    delay: float option // In minutes
    stopHistory: StopHistoryItem array option
    // These trip details should really be a variant of the full GTFS info, but
    // with tripStartDate added into the primary key. I don't see a way to do
    // that without a lot of copy-paste, so for now it's ad-hoc
    routeId: string
    shortName: string option
    // We could end up with different names for the same routeId anyway over
    // time, so let's embrace it and denormalise to buggery!
    routeShortName: string option
}

/// To be able to look back at historical data, which may contain since defunct
/// stops or stops which have since been moved, we store the history of stops as
/// well. Ideally each distinct stop (name and position) would have its own ID
/// (maybe a hash), but that would mean fetching and diffing stops each time trip
/// position are fetched. As a hopefully reasonable compromise, we store stops
/// coupled with a range of days, when the data was applicable. Realistically,
/// stops don't substantially change often and using the same IDs as static
/// timetables has a number of advantages.
/// TODO: Another option might be storing only an intermediate ID in the stop
/// history, which would point to an actual stop with a full stable ID, name and
/// position. However, that doesn't really map to how the APIs work. We'd need to
/// do something like check if a stop has changed before inserting each
/// StopHistoryItem.
type Stop = {
    id: string
    date: LocalDate
    name: string
    lat: float option
    lon: float option
}