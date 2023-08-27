// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfSerializer

open System
open NodaTime

open JrUtil.CsvSerializer
open JrUtil.JdfParser

let rec jdfColSerializerFor colType =
    if colType = typeof<LocalDate> then
        fun (x: obj) -> jdfDatePattern.Format(x :?> LocalDate)
    else if colType = typeof<LocalTime> then
        fun (x: obj) -> jdfTimePattern.Format(x :?> LocalTime)
    else
        colSerializerForBase jdfColSerializerFor colType

let getJdfSerializer<'r> =
    let rowSerializer = getRowSerializer<'r> jdfColSerializerFor
    fun (records: 'r seq) ->
        String.Join("\r\n",
            records
            |> Seq.map (fun r ->
                "\"" + String.Join("\",\"", rowSerializer (box r)) + "\""))
