// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsParser

open System
open System.IO
open System.Globalization

open JrUtil.CsvParser
open JrUtil.GtfsModelMeta

let rec gtfsColParserFor colType =
    if colType = typeof<DateTime> then
        dateTimeParser [|"yyyyMMdd"|]
    else if colType = typeof<TimeSpan> then
        fun instr ->
            let nums = instr.Split(":") |> Array.map int
            if nums.Length = 3
            then new TimeSpan(nums.[0], nums.[1], nums.[2]) |> box
            else raise (CsvParseException
                         (sprintf "Invalid value for TimeSpan: %s" instr))
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

let getGtfsParser<'r> =
    let rowParser = getRowParser<'r> gtfsColParserFor
    fun (text: string) ->
        let lines = text.Split("\n")
        let header = lines |> Array.head
        let data = lines |> Array.tail |> Array.filter (fun l -> l <> "")

        let colNames = splitLine header
        let modelHeader = getHeader typeof<'r>
        let transformToModel =
            let indexes =
                modelHeader
                |> Array.map (fun name ->
                    match colNames |> Seq.tryFindIndex (fun n -> n = name) with
                    | Some i -> i
                    | None -> failwithf "Column \"%s\" not in GTFS file" name
                )
            fun (cols: string seq) ->
                indexes |> Array.map (fun i -> cols |> Seq.item i)

        data
        |> Array.map (fun line ->
            splitLine line
            |> transformToModel
            |> Seq.toArray
            |> rowParser
        )

let getGtfsFileParser<'r> =
    let parser = getGtfsParser<'r>
    fun path ->
        let text = File.ReadAllText(path)
        parser text
