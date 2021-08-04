// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík
namespace JrUtil.Tests

open System
open System.Diagnostics
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime
open JrUtil.RealTimeModel
open JrUtil.UndatedTimes
open JrUtil.Tests.Asserts

[<TestClass>]
type UndatedStopHistoryTests() =

    [<TestMethod>]
    member this.``date undated times over one day``() =
        let input = [|
            LocalTime(23, 10)
            LocalTime(0, 0)
            LocalTime(0, 10)
            LocalTime(1, 0)
        |]
        let out = dateUndatedTimes 1 (LocalDateTime(2020, 01, 10, 1, 0)) input
        out |> assertSeqEqual [|
            LocalDateTime(2020, 01, 9, 23, 10)
            LocalDateTime(2020, 01, 10, 0, 0)
            LocalDateTime(2020, 01, 10, 0, 10)
            LocalDateTime(2020, 01, 10, 1, 0)
        |]

    [<TestMethod>]
    member this.``date undated optional times over one day``() =
        let input = [|
            None
            Some <| LocalTime(23, 10)
            Some <| LocalTime(0, 0)
            None
            Some <| LocalTime(0, 10)
            Some <| LocalTime(1, 0)
        |]
        let out =
            dateOptionalUndatedTimes
                2 (LocalDateTime(2020, 01, 10, 1, 0)) input
        out |> assertSeqEqual [|
            None
            Some <| LocalDateTime(2020, 01, 9, 23, 10)
            Some <| LocalDateTime(2020, 01, 10, 0, 0)
            None
            Some <| LocalDateTime(2020, 01, 10, 0, 10)
            Some <| LocalDateTime(2020, 01, 10, 1, 0)
        |]

    [<TestMethod>]
    member this.``date undated one-day trip``() =
        let input = [|
            {
                UndatedStopHistoryItem.stopId = "1"
                arrivedAt = None
                shouldArriveAt = None
                departedAt = Some <| LocalTime(9, 10)
                shouldDepartAt = Some <| LocalTime(9, 8)
            }
            {
                UndatedStopHistoryItem.stopId = "2"
                arrivedAt = Some <| LocalTime(10, 11)
                shouldArriveAt = Some <| LocalTime(10, 10)
                departedAt = None
                shouldDepartAt = Some <| LocalTime(10, 15)
            }
            {
                UndatedStopHistoryItem.stopId = "3"
                arrivedAt = Some <| LocalTime(10, 30)
                shouldArriveAt = Some <| LocalTime(10, 30)
                departedAt = Some <| LocalTime(10, 30)
                shouldDepartAt = Some <| LocalTime(10, 30)
            }
        |]
        let timezone = DateTimeZoneProviders.Tzdb.["Europe/Prague"]
        let out =
            dateUndatedStopHistory
                timezone 0 (LocalDateTime(2020, 01, 01, 10, 0)) input
        out |> assertSeqEqual [|
            {
                StopHistoryItem.stopId = "1"
                tripStopIndex = 1
                timeZone = timezone
                arrivedAt = None
                shouldArriveAt = None
                departedAt = Some <| LocalDateTime(2020, 1, 1, 9, 10)
                shouldDepartAt = Some <| LocalDateTime(2020, 1, 1, 9, 8)
            }
            {
                StopHistoryItem.stopId = "2"
                tripStopIndex = 2
                timeZone = timezone
                arrivedAt = Some <| LocalDateTime(2020, 1, 1, 10, 11)
                shouldArriveAt = Some <| LocalDateTime(2020, 1, 1, 10, 10)
                departedAt = None
                shouldDepartAt = Some <| LocalDateTime(2020, 1, 1, 10, 15)
            }
            {
                StopHistoryItem.stopId = "3"
                tripStopIndex = 3
                timeZone = timezone
                arrivedAt = Some <| LocalDateTime(2020, 1, 1, 10, 30)
                shouldArriveAt = Some <| LocalDateTime(2020, 1, 1, 10, 30)
                departedAt = Some <| LocalDateTime(2020, 1, 1, 10, 30)
                shouldDepartAt = Some <| LocalDateTime(2020, 1, 1, 10, 30)
            }
        |]

    [<TestMethod>]
    member this.``date undated two-day trip``() =
        let input = [|
            {
                UndatedStopHistoryItem.stopId = "1"
                arrivedAt = None
                shouldArriveAt = None
                departedAt = Some <| LocalTime(23, 10)
                shouldDepartAt = Some <| LocalTime(23, 0)
            }
            {
                UndatedStopHistoryItem.stopId = "2"
                arrivedAt = Some <| LocalTime(0, 10)
                shouldArriveAt = Some <| LocalTime(0, 0)
                departedAt = Some <| LocalTime(0, 10)
                shouldDepartAt = Some <| LocalTime(0, 5)
            }
            {
                UndatedStopHistoryItem.stopId = "3"
                arrivedAt = Some <| LocalTime(2, 0)
                shouldArriveAt = Some <| LocalTime(2, 0)
                departedAt = Some <| LocalTime(2, 20)
                shouldDepartAt = Some <| LocalTime(2, 10)
            }
        |]
        let timezone = DateTimeZoneProviders.Tzdb.["Europe/Prague"]
        let out =
            dateUndatedStopHistory
                timezone 2 (LocalDateTime(2020, 01, 10, 3, 0)) input
        out |> assertSeqEqual [|
            {
                StopHistoryItem.stopId = "1"
                tripStopIndex = 1
                timeZone = timezone
                arrivedAt = None
                shouldArriveAt = None
                departedAt = Some <| LocalDateTime(2020, 1, 9, 23, 10)
                shouldDepartAt = Some <| LocalDateTime(2020, 1, 9, 23, 0)
            }
            {
                StopHistoryItem.stopId = "2"
                tripStopIndex = 2
                timeZone = timezone
                arrivedAt = Some <| LocalDateTime(2020, 1, 10, 0, 10)
                shouldArriveAt = Some <| LocalDateTime(2020, 1, 10, 0, 0)
                departedAt = Some <| LocalDateTime(2020, 1, 10, 0, 10)
                shouldDepartAt = Some <| LocalDateTime(2020, 1, 10, 0, 5)
            }
            {
                StopHistoryItem.stopId = "3"
                tripStopIndex = 3
                timeZone = timezone
                arrivedAt = Some <| LocalDateTime(2020, 1, 10, 2, 0)
                shouldArriveAt = Some <| LocalDateTime(2020, 1, 10, 2, 0)
                departedAt = Some <| LocalDateTime(2020, 1, 10, 2, 20)
                shouldDepartAt = Some <| LocalDateTime(2020, 1, 10, 2, 10)
            }
        |]
