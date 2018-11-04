// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsModelMeta

open System
open System.Reflection
open Microsoft.FSharp.Reflection

open JrUtil

[<AllowNullLiteral>]
type CsvSpreadAttribute(headers: string array) =
    inherit CsvParser.CsvSpreadAttribute(headers.Length)
    member this.Headers = headers

[<AllowNullLiteral>]
type CsvFieldNameAttribute(name: string) =
    inherit Attribute()
    member this.Name = name

let getFieldColumnNames (field: PropertyInfo) =
    let spreadAttr = field.GetCustomAttribute<CsvSpreadAttribute>()
    let nameAttr = field.GetCustomAttribute<CsvFieldNameAttribute>()
    assert ((spreadAttr = null) <> (nameAttr = null))
    if spreadAttr <> null then spreadAttr.Headers
    else [| nameAttr.Name |]

let getHeader recType =
    assert FSharpType.IsRecord(recType)
    FSharpType.GetRecordFields(recType)
    |> Array.collect getFieldColumnNames
