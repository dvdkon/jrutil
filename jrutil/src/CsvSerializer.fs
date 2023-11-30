// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CsvSerializer

open System
open System.Reflection
open Microsoft.FSharp.Reflection

open JrUtil.ReflectionUtils
open JrUtil.UnionCodec
open JrUtil.CsvMetadata

let rec colSerializerForBase colSerializerFor (colType: Type) =
    let serializeMethod = colType.GetMethod("CsvSerialize")
    if serializeMethod <> null then fun x ->
        serializeMethod.Invoke(x, [||]) :?> string
    else if colType = typeof<bool> then
        fun (x: obj) -> if x :?> bool then "1" else "0"
    else if typeIsOption colType then
        let innerType = colType.GetGenericArguments().[0]
        let innerTypeSerializer = colSerializerFor innerType
        let isSomeProperty = colType.GetProperty("IsSome")
        let valueProperty = colType.GetProperty("Value")
        fun xo ->
            if isSomeProperty.GetValue(null, [|xo|]) :?> bool then
                valueProperty.GetValue(xo)
                |> innerTypeSerializer
            else ""
    else if FSharpType.IsUnion(colType) then getUnionSerializer colType
    else string

let getRowSerializer<'r> (colSerializerFor: Type -> (obj -> string)) =
    let recordType = typeof<'r>
    assert FSharpType.IsRecord(recordType)
    let serializers =
        FSharpType.GetRecordFields(recordType)
        |> Array.map (fun field ->
            let spreadAttr = field.GetCustomAttribute<CsvSpreadAttribute>()
            if spreadAttr <> null then
                assert field.PropertyType.IsArray
                let serializer =
                    colSerializerFor (field.PropertyType.GetElementType())
                fun r ->
                    let arr = field.GetValue(r) :?> Array
                    seq {
                        for i in 0..spreadAttr.Len - 1 ->
                            if i < arr.Length
                            then serializer <| arr.GetValue(i)
                            else ""
                    }
            else
                let serializer = colSerializerFor field.PropertyType
                fun r -> seq { field.GetValue(r) |> serializer })

    fun record ->
        serializers
        |> Seq.collect (fun s -> s record)
