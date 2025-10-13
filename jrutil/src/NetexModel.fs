// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2024 David Koňařík

module JrUtil.NetexModel

open System.Xml
open System.Xml.Linq

open JrUtil.UnionCodec
open JrUtil.NetexModelMeta
open JrUtil.NetexSerializer

type Version =
    | Any
    | Version of string
with
    override this.ToString() =
        match this with
        | Any -> "any"
        | Version v -> v

type Reference = {
    id: string
    version: Version
}

type Ref<'t> =
    | Embedded of 't
    // ID and version specifier
    | Referenced of Reference

[<XmlTransparent>]
type XmlExtra = {
    attrs: XAttribute array
    elems: XElement array
}
with
    static member Empty = { attrs = [||]; elems = [||] }
    interface ICustomXmlSerializer with
        member this.SerializeAttrs(w: XmlWriter) =
            for a in this.attrs do
                // TODO: Attribute namespaces?
                w.WriteAttributeString(a.Name.LocalName, a.Value)
        member this.SerializeContent(w) =
            for e in this.elems do e.WriteTo(w)

[<XmlTransparent>]
type DataManagedObject = {
    [<XmlAttr>]
    id: string option
    [<XmlAttr>]
    version: Version option
    // Other attributes are treated as extra attrs
}

[<XmlNamespace("http://www.opengis.net/gml/3.2")>]

type GmlPos = { srd: string; x: double; y: double }
with static member Serializer = getSerializer typeof<GmlPos>

type Location = {
    latitude: decimal option
    longitude: decimal option
    altitude: decimal option
    pos: GmlPos option
    precision: decimal option
}
with
    static member FromLatLon(lat, lon) = {
        latitude = Some lat
        longitude = Some lon
        altitude = None
        pos = None
        precision = None
    }

type Centroid = {
    location: Location
}

type Codespace = {
    extra: XmlExtra

    [<XmlAttr>]
    id: string
    xmlns: string
    xmlnsUrl: string option
    description: string option
}

type TariffZone = {
    obj: DataManagedObject
    extra: XmlExtra

    name: string option
}

type QuayType =
    | [<StrValue("airlineGate")>] AirlineGate
    | [<StrValue("railPlatform")>] RailPlatform
    | [<StrValue("metroPlatform")>] MetroPlatform
    | [<StrValue("coachStop")>] CoachStop
    | [<StrValue("busStop")>] BusStop
    | [<StrValue("busPlatform")>] BusPlatform
    | [<StrValue("busBay")>] BusBay
    | [<StrValue("tramPlatform")>] TramPlatform
    | [<StrValue("tramStop")>] TramStop
    | [<StrValue("boatQuay")>] BoatQuay
    | [<StrValue("ferryLanding")>] FerryLanding
    | [<StrValue("telecabinePlatform")>] TelecabinePlatform
    | [<StrValue("taxiStand")>] TaxiStand
    | [<StrValue("setDownPlace")>] SetDownPlace
    | [<StrValue("vehicleLoadingPlace")>] VehicleLoadingPlace
    | [<StrValue("multimodal")>] Multimodal
    | [<StrValue("other")>] Other

type Quay = {
    obj: DataManagedObject
    extra: XmlExtra

    publicCode: string option
    quayType: QuayType option
}

type StopType =
    | [<StrValue("onstreetBus")>] OnstreetBus
    | [<StrValue("onstreetTram")>] OnstreetTram
    | [<StrValue("airport")>] Airport
    | [<StrValue("railStation")>] RailStation
    | [<StrValue("metroStation")>] MetroStation
    | [<StrValue("busStation")>] BusStation
    | [<StrValue("coachStation")>] CoachStation
    | [<StrValue("tramStation")>] TramStation
    | [<StrValue("harbourPort")>] HarbourPort
    | [<StrValue("ferryPort")>] FerryPort
    | [<StrValue("ferryStop")>] FerryStop
    | [<StrValue("liftStation")>] LiftStation

...

    | [<StrValue("vehicleRailInterchange")>] VehicleRailInterchange
    | [<StrValue("taxiRank")>] TaxiRank
    | [<StrValue("other")>] Other

type StopPlace = {
    obj: DataManagedObject
    extra: XmlExtra

    name: string option
    centroid: Centroid option
    publicCode: string option
    stopPlaceType: StopType option
    quays: Quay array
}

type SiteFrame = {
    obj: DataManagedObject
    extra: XmlExtra

    stopPlaces: StopPlace array
    tariffZones: TariffZone array
}

type ScheduledStopPoint = {
    obj: DataManagedObject
    extra: XmlExtra

    name: string option
    location: Location option
    publicCode: string option
    stopType: StopType option
}

type HolidayType =
    | [<StrValue("AnyDay")>] AnyDay
    | [<StrValue("WorkingDay")>] WorkingDay
    | [<StrValue("SchoolDay")>] SchoolDay
    | [<StrValue("NotHoliday")>] NotHoliday
    | [<StrValue("NotWorkingDay")>] NotWorkingDay
    | [<StrValue("NotSchoolDay")>] NotSchoolDay
    | [<StrValue("AnyHoliday")>] AnyHoliday
    | [<StrValue("LocalHoliday")>] LocalHoliday
    | [<StrValue("RegionalHoliday")>] RegionalHoliday
    | [<StrValue("NationalHoliday")>] NationalHoliday
    | [<StrValue("HolidayDisplacementDay")>] HolidayDisplacementDay
    | [<StrValue("EveOfHoliday")>] EveOfHoliday

type Season =
    | [<StrValue("Spring")>] Spring
    | [<StrValue("Summer")>] Summer
    | [<StrValue("Autumn")>] Autumn
    | [<StrValue("Winter")>] Winter
    | [<StrValue("Perennially")>] Perennially

type DayOfWeek =
    | [<StrValue("Monday")>] Monday
    | [<StrValue("Tuesday")>] Tuesday
    | [<StrValue("Wednesday")>] Wednesday
    | [<StrValue("Thursday")>] Thursday
    | [<StrValue("Friday")>] Friday
    | [<StrValue("Saturday")>] Saturday
    | [<StrValue("Sunday")>] Sunday
    | [<StrValue("Everyday")>] Everyday
    | [<StrValue("Weekdays")>] Weekdays
    | [<StrValue("Weekend")>] Weekend
    | [<StrValue("none")>] NoDays

type WeekOfMonth =
    | [<StrValue("1")>] Week1
    | [<StrValue("2")>] Week2
    | [<StrValue("3")>] Week3
    | [<StrValue("4")>] Week4
    | [<StrValue("5")>] Week5
    | [<StrValue("EveryWeek")>] EveryWeek

type PropertyOfDay = {
    extra: XmlExtra

    daysOfWeek: DayOfWeek array option
    weeksOfMonth: WeekOfMonth array option
    // TODO: Parse
    monthOfYear: string option
    dayOfMonth: string option
    dayOfYear: string option

    // TODO: CountryRef
    // Ignoring tides, dayevent, crowding
    holidayTypes: HolidayType array option
    seasons: Season array option
}

type DayType = {
    obj: DataManagedObject

    extra: XmlExtra

    name: string option
    properties: PropertyOfDay array option
}

type AllVehicleModesOfTransport =
    | [<StrValue("all")>] All
    | [<StrValue("unknown")>] Unknown
    | [<StrValue("bus")>] Bus
    | [<StrValue("trolleyBus")>] TrolleyBus
    | [<StrValue("tram")>] Tram
    | [<StrValue("coach")>] Coach
    | [<StrValue("rail")>] Rail
    | [<StrValue("intercityRail")>] IntercityRail
    | [<StrValue("urbanRail")>] UrbanRail
    | [<StrValue("metro")>] Metro
    | [<StrValue("air")>] Air
    | [<StrValue("water")>] Water
    | [<StrValue("cableway")>] Cableway
    | [<StrValue("funicular")>] Funicular
    | [<StrValue("snowAndIce")>] SnowAndIce
    | [<StrValue("taxi")>] Taxi
    | [<StrValue("ferry")>] Ferry
    | [<StrValue("lift")>] Lift
    | [<StrValue("selfDrive")>] SelfDrive
    | [<StrValue("anyMode")>] AnyMode
    | [<StrValue("other")>] Other

type Line = {
    obj: DataManagedObject
    extra: XmlExtra

    name: string

...

    transportMode: AllVehicleModesOfTransport option
    publicCode: string option
}

type RoutePoint = {
    obj: DataManagedObject
    extra: XmlExtra

    location: Location
}

type PointOnRoute = {
    obj: DataManagedObject // Actually Entity, but do we care?

    [<XmlAttr>]
    order: int
    //linkSequence: LinkSequence Ref option
    routePoint: RoutePoint Ref
}

type Route = {
    obj: DataManagedObject
    extra: XmlExtra

    line: Line Ref option
    pointsInSequence: PointOnRoute array
}

type ServiceJourneyPattern = {
    obj: DataManagedObject
    extra: XmlExtra

    // TODO: Optionally replaced by RouteView
    route: Route Ref
}

type ServiceJourney = {
    obj: DataManagedObject
    extra: XmlExtra

    serviceJourneyPattern: ServiceJourneyPattern Ref
    service}
}
