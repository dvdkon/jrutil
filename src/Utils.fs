// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Utils

open System
open Microsoft.FSharp.Reflection

[<AllowNullLiteral>]
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

let getUnionParser unionType =
    let cases = getUnionStrCases unionType
    fun x ->
        let cases =
            cases
            |> Array.filter (fun (c, v) -> v = x)
            |> Array.map (fun (c, v) -> c)
        if cases.Length <> 1
        then failwithf "No union case for value \"%s\"" x
        FSharpValue.MakeUnion(cases.[0], [||])

let parseUnion<'u> str =
    // This doesn't cache the parser function, I just hope the JIT takes care
    // of that
    (getUnionParser typeof<'u>) str |> unbox<'u>


let getUnionSerializer unionType =
    let cases = getUnionStrCases unionType
    fun x ->
        let (case, fields) = FSharpValue.GetUnionFields(x, unionType)
        let (_, caseStr) =
            cases
            |> Array.find (fun (ci, _) -> ci = case)
        caseStr



let rec dateRange (startDate: DateTime) (endDate: DateTime)  =
    // Create a list of DateTime objects containing all days between
    // startDate and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateRange (startDate.AddDays(1.0)) endDate)
    else []
