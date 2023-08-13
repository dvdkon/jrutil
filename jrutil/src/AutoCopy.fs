// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.AutoCopy

open System.Reflection
open Microsoft.FSharp.Reflection

// A helper functions that uses reflection to copy data from an object of one
// type to an object of a similar type.
let getAutoCopier<'f, 't> getterGetter =
    let fromType = typeof<'f>
    let toType = typeof<'t>

    if FSharpType.IsRecord(fromType) then
        assert FSharpType.IsRecord(toType)
        let toConstructor = FSharpValue.PreComputeRecordConstructor(toType)
        let fromFields = FSharpType.GetRecordFields(fromType)
        let toFields = FSharpType.GetRecordFields(toType)
        let fieldGetters =
            toFields
            |> Seq.map (fun field ->
                match getterGetter field with
                | Some g -> g
                | None ->
                    let fromField =
                        fromFields
                        |> Seq.tryFind (fun f -> f.Name = field.Name)
                    match fromField with
                    | Some field -> fun fromObj -> field.GetValue(fromObj)
                    | None ->
                        failwithf "Found no source for field \"%s\"" field.Name
            )
        fun (fromObj: 'f) ->
            let fieldVals = fieldGetters |> Seq.map (fun f -> f fromObj)
            toConstructor(fieldVals |> Seq.toArray) |> unbox<'t>
    else
        unbox

// Helper function that transforms a list of getters for fields into a
// "getter getter" for "getAutoCopier"
let nameGetters (gs: (string * ('f -> obj)) list) =
    let gsMap = Map gs
    fun (f: PropertyInfo) ->
        Map.tryFind f.Name gsMap
