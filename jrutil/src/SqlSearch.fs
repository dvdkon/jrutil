// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.SqlSearch

open System
open System.Collections.Generic
open FsToolkit.ErrorHandling

open JrUtil.Utils
open JrUtil.SearchParser

type ParamAllocator() =
    let mutable nextParam = 1
    member val paramMap: (string * obj) ResizeArray = ResizeArray() with get

    member this.add(value: 'a) =
        let name = $"p{nextParam}"
        nextParam <- nextParam + 1
        this.paramMap.Add((name, box value))
        $"@{name}"

type SearchFieldImpl = ParamAllocator -> Expression list -> Result<string, string>

let simpleField field (pa: ParamAllocator) args =
    args
    |> List.map (fun a ->
        match a with
        | Text s -> Ok s
        | other -> Error $"Simple field got non-text arg: {other}")
    |> List.sequenceResultM
    |> Result.bind (field pa)

let stringField name = simpleField <| fun pa args ->
    match args with
    | ["contains"; str] | [str] ->
        let likeStr = "%" + str + "%"
        Ok $"{name} ILIKE {pa.add(likeStr)}"
    | ["eq"; str] -> Ok $"LOWER({name}) = LOWER({pa.add(str)})"
    | _ -> Error "Invalid string field invocation"

let numericField parser name =
    simpleField <| fun pa args -> result {
        let! op, str =
            match args with
            | [op; str] -> Ok (op, str)
            | [str] -> Ok ("eq", str)
            | _ -> Error "Invalid int field invocation"
        let! number = parser str
        return! match op with
                | "eq" -> Ok $"{name} = {pa.add(number)}"
                | "lt" -> Ok $"{name} < {pa.add(number)}"
                | "gt" -> Ok $"{name} > {pa.add(number)}"
                | _ -> Error "Invalid string field invocation"
    }

let intField: string -> SearchFieldImpl = numericField <| fun str ->
    match Int64.TryParse(str) with
    | false, _ -> Error "Failed parsing integer"
    | true, v -> Ok v

let dateField: string -> SearchFieldImpl = numericField <| fun str ->
    tryParseDate "yyyy-MM-dd" str
    |> optResult "Invalid date format, has to be yyyy-mm-dd"

let timeField: string -> SearchFieldImpl = numericField <| fun str ->
    tryParseTime "H:mm" str
    |> optResult "Invalid time format, has to be h:mm"

let periodField: string -> SearchFieldImpl = numericField <| fun str ->
    tryParsePeriod @"hh\:mm\:ss" str
    |> optResult "Invalid period format, has to be hh:mm:ss"

let applyField fields pa args =
        match List.head args with
        | Text field ->
            fields
            |> Map.tryFind field
            |> optResult $"No such field: {field}"
            |> Result.bind (fun f -> f pa (List.tail args))
        | _ -> Error $"Invalid field name: {List.head args}"

let rec clauseToSql (fields: Map<string, SearchFieldImpl>) paramAlloc clause =
    match clause with
    | Text _ as t -> fields.[""] paramAlloc [t]
    | Application args -> applyField fields paramAlloc args

    | Composite [] -> Ok "TRUE"
    | Composite inner ->
        inner
        |> List.map (clauseToSql fields paramAlloc)
        |> List.sequenceResultA
        |> Result.map (fun sqls ->
            String.Join(" AND ", sqls |> List.map (fun s -> $"({s})")))
        |> Result.mapError (fun es -> String.Join("; ", es))

let innerArgsToSql fields pa = function
    | [c] -> clauseToSql fields pa c
    | Text _ :: _ as args ->
        clauseToSql fields pa (Application args)
    | _ -> Error "Invalid child expression"

let notField fieldsGetter pa args =
    innerArgsToSql (fieldsGetter ()) pa args
    |> Result.map (sprintf "NOT (%s)")


let searchToSql (fields: Map<string, SearchFieldImpl>)
                (clause: Expression) =
    let paramAlloc = ParamAllocator()
    clauseToSql fields paramAlloc clause
    |> Result.map (fun s -> s, paramAlloc.paramMap)
