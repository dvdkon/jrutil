// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.UnionCodec

open System
open Microsoft.FSharp.Reflection

open JrUtil.Utils

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.All, AllowMultiple=true)>]
type StrValueAttribute(value: string) =
    inherit Attribute()
    member this.Value = value

let getUnionStrCases unionType =
    FSharpType.GetUnionCases(unionType)
    |> Array.collect (fun c ->
        c.GetCustomAttributes()
        |> Array.filter (fun a -> a :? StrValueAttribute)
        |> Array.map (fun a -> a :?> StrValueAttribute)
        |> Array.map (fun a -> (c, a.Value)))

let getUnionParser = memoize <| fun unionType ->
    let unionConstructors =
        getUnionStrCases unionType
        |> Array.map
            (fun (c, a) -> (a, FSharpValue.PreComputeUnionConstructor(c)))
        |> Map
    fun x ->
        match unionConstructors |> Map.tryFind x with
        | Some c -> c [| |]
        | None -> failwithf "No union case for value \"%s\"" x

let parseUnion<'u> str =
    (getUnionParser typeof<'u>) str |> unbox<'u>

let getUnionSerializer = memoize <| fun (unionType: Type) ->
    let cases =
        getUnionStrCases unionType
        |> Array.map (fun (c, a) -> (c.Tag, a))
    let caseTagGetter = FSharpValue.PreComputeUnionTagReader unionType
    fun (x: obj) ->
        let caseTag = caseTagGetter x
        let (_, caseStr) =
            cases
            |> Array.tryFind (fun (ci, _) -> ci = caseTag)
            |> Option.defaultWith (fun () ->
                failwithf "No string value for union case %A of %A"
                          caseTag unionType)
        caseStr

let serializeUnion<'u> (u: 'u) =
    (getUnionSerializer typeof<'u>) u
