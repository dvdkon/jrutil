// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsCsvSerializer

open System
open System.Reflection
open Microsoft.FSharp.Reflection

open JrUtil.Utils

[<AllowNullLiteral>]
type CsvSpreadAttribute(headers: string array) =
    inherit Attribute()
    member this.Headers = headers

[<AllowNullLiteral>]
type CsvFieldNameAttribute(name: string) =
    inherit Attribute()
    member this.Name = name

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
            [| ts.ToString(@"hh\:mm\:ss") |]
    else if fieldType.IsGenericType
            && fieldType.GetGenericTypeDefinition() = typedefof<_ option> then
        let innerType = fieldType.GetGenericArguments().[0]
        let innerFormatter = getFormatter innerType
        fun x ->
            let (_, fields) = FSharpValue.GetUnionFields(x, fieldType)
            // I could test the union case, but it's much easier to just
            // test the field array length
            match fields.Length with
            | 0 -> [| "" |]
            | 1 -> innerFormatter (unbox fields.[0])
            // The typechecker won't accept assert :(
            | _ -> failwith "Impossible"
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
        fun x -> [| getUnionSerializer fieldType x |]
    else (fun x -> [| sprintf "%A" x |])

let getFieldColumnNames (field: PropertyInfo) =
    let spreadAttr = field.GetCustomAttribute<CsvSpreadAttribute>()
    let nameAttr = field.GetCustomAttribute<CsvFieldNameAttribute>()
    assert ((spreadAttr = null) <> (nameAttr = null))
    if spreadAttr <> null then spreadAttr.Headers
    else [| nameAttr.Name |]

let getHeader<'r> =
    assert FSharpType.IsRecord(typeof<'r>)
    FSharpType.GetRecordFields(typeof<'r>)
    |> Array.collect getFieldColumnNames
    |> String.concat ","

let getSerializer<'r> =
    assert FSharpType.IsRecord(typeof<'r>)
    let formatters =
        FSharpType.GetRecordFields(typeof<'r>)
        |> Array.map (fun f -> getFormatter f.PropertyType)
    fun (row: 'r) ->
        FSharpValue.GetRecordFields(row)
        |> Array.zip formatters
        |> Array.collect (fun (f, v) -> f v)
        |> Array.map (fun c -> sprintf "\"%s\"" (c.Replace("\"","\"\"")))
        |> String.concat ","

let serializeRows<'r> rows =
    let header = getHeader<'r>
    let serializer = getSerializer<'r>
    header + "\n" + (rows |> Seq.map serializer |> String.concat "\n")
