// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfSerializer

open System
open NodaTime

open JrUtil.CsvSerializer
open JrUtil.JdfParser
open System.IO

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
                "\"" + String.Join("\",\"", rowSerializer (box r)) + "\";"))
        + "\r\n"

let getJdfSerializerWriter<'r> =
    let rowSerializer = getRowSerializer<'r> jdfColSerializerFor
    fun (stream: Stream) (records: 'r seq) ->
        use writer = new StreamWriter(stream, jdfEncoding)
        for record in records do
            writer.Write("\"")
            let cols = rowSerializer record
            let enum = cols.GetEnumerator()
            let mutable reading = enum.MoveNext()
            while reading do
                writer.Write(enum.Current)
                if enum.MoveNext() then
                    writer.Write("\",\"")
                else
                    reading <- false
            writer.Write("\";\r\n")
