// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module JrUtil.GtfsParser

open System
open Microsoft.FSharp.Reflection
open NodaTime

open JrUtil.Utils
open JrUtil.ReflectionUtils
open JrUtil.CsvParser
open JrUtil.GtfsModelMeta

let rec gtfsColParserFor colType =
    if colType = typeof<LocalDate> then
        parseDate "yyyyMMdd" >> box
    else if colType = typeof<Period> then
        fun instr ->
            let nums = instr.Split(":") |> Array.map int64
            if nums.Length = 3 then
                let secs = nums.[0] * 3600L + nums.[1] * 60L + nums.[2]
                Period.FromSeconds(secs) |> box
            else raise (CsvParseException
                         (sprintf "Invalid value for Period: %s" instr))
    else
        colParserForBase gtfsColParserFor colType

type MList<'T> = System.Collections.Generic.List<'T>

let splitLine (line: string) =
    let mutable quoted = false
    let cols = new MList<MList<char>>()
    cols.Add(new MList<char>())
    let chars = line.ToCharArray()
    chars
    |> Array.iteri (fun i c ->
        let nextc = chars |> Array.tryItem (i + 1)
        let lastCol = cols.[cols.Count - 1]
        match c with
        | ',' when not quoted -> cols.Add(new MList<char>())
        | '"' when not quoted -> quoted <- true
        | '"' when quoted && nextc = Some '"' ->
            lastCol.Add('"')
        | '"' when quoted
                && lastCol.Count > 0
                && lastCol.[lastCol.Count - 1] = '"' -> ()
        | '"' when quoted -> quoted <- false
        | _ -> lastCol.Add(c)
    )
    cols
    |> Seq.map (fun col -> String.Join("", col))

let gtfsTransformToModel rowType header =
    let colNames = splitLine header
    let modelHeader = getHeader rowType
    let fieldTypes =
        FSharpType.GetRecordFields(rowType)
        |> Array.map (fun f -> f.PropertyType)
    let indexes =
        modelHeader
        |> Array.mapi (fun i name ->
            match colNames |> Seq.tryFindIndex (fun n -> n = name) with
            | Some j -> Some j
            | None -> if typeIsOption fieldTypes.[i]
                      then None
                      else failwithf "Column \"%s\" not in GTFS file" name
        )
    fun (cols: string seq) ->
        indexes |> Array.map (function
                              | Some i -> cols |> Seq.item i
                              | None -> "")

let getGtfsParser<'r> =
    let rowParser = getRowParser<'r> gtfsColParserFor
    fun (lines: string seq) ->
        let header = lines |> Seq.head
        let data = lines |> Seq.tail |> Seq.filter (fun l -> l <> "")
        let transformToModel = gtfsTransformToModel typeof<'r> header

        data |> Seq.map (splitLine >> transformToModel >> rowParser)

let getGtfsFileParser<'r> =
    let parser = getGtfsParser<'r>
    fun path ->
        let text = fileLinesSeq path
        parser text
