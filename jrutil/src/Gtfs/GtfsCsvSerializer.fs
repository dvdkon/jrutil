// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsCsvSerializer

open System
open Microsoft.FSharp.Reflection

open JrUtil.ReflectionUtils
open JrUtil.UnionCodec
open JrUtil.GtfsModelMeta

let optionSomeCase =
    FSharpType.GetUnionCases(typedefof<_ option>)
    |> Array.find (fun uc -> uc.Name = "Some")
let rec getFormatter fieldType =
    if fieldType = typeof<string> then (fun x -> [| unbox x |])
    else if fieldType = typeof<int> then (fun x -> [| sprintf "%d" (unbox x) |])
    else if fieldType = typeof<decimal> then
        (fun x -> [| sprintf "%M" (unbox x) |])
    else if fieldType = typeof<bool> then
        fun x ->
            match unbox x with
            | true -> [| "1" |]
            | false -> [| "0" |]
    else if fieldType = typeof<DateTime> then
        fun x ->
            let dt: DateTime = unbox x
            [| dt.ToString("yyyyMMdd") |]
    else if fieldType = typeof<TimeSpan> then
        fun x ->
            let ts: TimeSpan = unbox x
            [| sprintf "%02d:%02d:%02d" (int ts.TotalHours)
                                        ts.Minutes
                                        ts.Seconds |]
    else if typeIsOption fieldType then
        let innerType = fieldType.GetGenericArguments().[0]
        let innerFormatter = getFormatter innerType
        fun x ->
            let case = optionCaseGetter x
            if case = optionSomeCase.Tag
            then innerFormatter (x.GetType().GetProperty("Value").GetValue(x))
            else [| "" |]
    else if fieldType.IsArray then
        let innerType = fieldType.GetElementType()
        let innerFormatter = getFormatter innerType
        fun x ->
            let arr: Array = unbox x
            let mutable resArr = Array.create arr.Length ([| "" |])
            for i = 0 to arr.Length - 1 do
                resArr.[i] <- innerFormatter (arr.GetValue(i))

            Array.concat resArr
    else if FSharpType.IsUnion(fieldType) then
        let serializer = getUnionSerializer fieldType
        fun x -> [| serializer x |]
    else (fun x -> [| sprintf "%A" x |])

let getSerializer<'r> =
    assert FSharpType.IsRecord(typeof<'r>)
    let formatters =
        FSharpType.GetRecordFields(typeof<'r>)
        |> Array.map (fun f -> getFormatter f.PropertyType)
    let fieldGetter = FSharpValue.PreComputeRecordReader(typeof<'r>)

    fun (row: 'r) ->
        fieldGetter(row)
        |> Array.zip formatters
        |> Array.collect (fun (f, v) -> f v)
        |> Array.map (fun c -> sprintf "\"%s\"" (c.Replace("\"","\"\"")))
        |> String.concat ","

let getRowsSerializer<'r> =
    let header = getHeader<'r> |> String.concat ","
    let serializer = getSerializer<'r>
    fun rows ->
        header + "\n" + (rows |> Seq.map serializer |> String.concat "\n")
