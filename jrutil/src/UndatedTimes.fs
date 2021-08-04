// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík

module JrUtil.UndatedTimes

open NodaTime
open JrUtil.RealTimeModel

let dateUndatedTimes
        fixedDateIdx
        (fixedDate: LocalDateTime)
        (times: LocalTime array) =
    let daysBeforeFixedOffset =
        // The value at fixedDateIdx was obtained at fixedDate, so it should be
        // in the same day. However, if the time part is later than the time
        // part of fixedDate, it must have been the day before
        if times.[fixedDateIdx] > fixedDate.TimeOfDay
        then 1
        else 0

    let daysBeforeFixed =
        (times
         |> Array.take (fixedDateIdx + 1)
         |> Array.pairwise
         |> Array.sumBy (fun (t1, t2) -> if t1 > t2 then 1 else 0))
        + daysBeforeFixedOffset

    times
    |> Seq.scan (fun (last: LocalDateTime) time ->
        if time < last.TimeOfDay then last.Date.Plus(Period.FromDays(1)) + time
        else last.Date + time
    ) (fixedDate.Plus(Period.FromDays(-daysBeforeFixed)).Date + LocalTime(0, 0))
    |> Seq.tail
    |> Seq.toArray

let dateOptionalUndatedTimes
        fixedDateIdx
        (fixedDate: LocalDateTime)
        (times: LocalTime option seq) =
    let beforeFixedDate =
        times
        |> Seq.take fixedDateIdx
        |> Seq.choose id
    let afterFixedDate =
        times
        |> Seq.skip fixedDateIdx
        |> Seq.choose id
    let someUndated =
        Seq.concat [beforeFixedDate; afterFixedDate]
        |> Seq.toArray
    let someDated = 
        dateUndatedTimes
            (Seq.length beforeFixedDate)
            fixedDate
            someUndated
    times
    |> Seq.mapFold (fun i t ->
        match t with
        | Some _ -> Some someDated.[i], i + 1
        | None -> None, i) 0
    |> fst

type UndatedStopHistoryItem = {
    stopId: string
    arrivedAt: LocalTime option
    shouldArriveAt: LocalTime option
    departedAt: LocalTime option
    shouldDepartAt: LocalTime option
}

let dateUndatedStopHistory
        timeZone
        fixedDateIdx
        (fixedDate: LocalDateTime)
        (undated: UndatedStopHistoryItem seq) =

    let rtTimes =
        undated
        |> Seq.collect (fun item ->
            [item.arrivedAt; item.departedAt])
        |> Seq.toArray
        |> dateOptionalUndatedTimes fixedDateIdx fixedDate
        |> Seq.toArray

    let ttTimes =
        undated
        |> Seq.collect (fun item ->
            [item.shouldArriveAt; item.shouldDepartAt])
        |> Seq.toArray
        |> dateOptionalUndatedTimes fixedDateIdx fixedDate
        |> Seq.toArray

    undated
    |> Seq.mapi (fun i item ->
        {
            StopHistoryItem.stopId = item.stopId
            tripStopIndex = i + 1
            timeZone = timeZone
            arrivedAt = rtTimes.[i*2]
            shouldArriveAt = ttTimes.[i*2]
            departedAt = rtTimes.[i*2 + 1]
            shouldDepartAt = ttTimes.[i*2 + 1]
        }
    )
