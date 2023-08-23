// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsCsvSerializer

open System
open System.Globalization
open Microsoft.FSharp.Reflection
open NodaTime

open JrUtil.ReflectionUtils
open JrUtil.UnionCodec
open JrUtil.GtfsModelMeta

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
    else if fieldType = typeof<LocalDate> then
        fun x ->
            let ld: LocalDate = unbox x
            [| ld.ToString("yyyyMMdd", CultureInfo.InvariantCulture) |]
    else if fieldType = typeof<Period> then
        fun x ->
            let p = (unbox<Period> x).Normalize()
            [| sprintf "%02d:%02d:%02d" (p.Hours + (int64 p.Days * 24L))
                                         p.Minutes
                                         p.Seconds |]
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
    let header = getHeader typeof<'r> |> String.concat ","
    let serializer = getSerializer<'r>
    fun rows ->
        header + "\n" + (rows |> Seq.map serializer |> String.concat "\n")
