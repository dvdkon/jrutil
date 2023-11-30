// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfParser

open System
open System.IO
open System.IO.Compression
open System.Text
open NodaTime
open NodaTime.Text

open JrUtil.Utils
open JrUtil.CsvParser

let jdfDatePattern = LocalDatePattern.CreateWithInvariantCulture("ddMMyyyy")
let jdfTimePattern = LocalTimePattern.CreateWithInvariantCulture("HHmm")

let rec jdfColParserFor colType =
    if colType = typeof<LocalDate> then
        fun s -> jdfDatePattern.Parse(s).GetValueOrThrow() |> box
    else if colType = typeof<LocalTime> then
        fun s -> jdfTimePattern.Parse(s).GetValueOrThrow() |> box
    else
        colParserForBase jdfColParserFor colType

let jdfEncoding = CodePagesEncodingProvider.Instance.GetEncoding(1250)

let getJdfParser<'r> =
    // The spec says "quotes inside text don't need to be doubled",
    // which is really confusing from an escaping standpoint
    // I take that to mean that there is really no quotation, and that
    // "," itself is the field separator (excluding the start (") and
    // the end (";)) and that it's unescapable
    // This, however, means that this CSV-esque format can be parsed by regex!
    let rowParser = getRowParser<'r> jdfColParserFor
    fun (stream: Stream) -> seq {
        use reader = new StreamReader(stream, jdfEncoding)
        let buffer = Array.create (1024*1024) ' '
        let mutable text = ""
        while not reader.EndOfStream do
            let len = reader.ReadBlock(buffer)
            text <- text + String buffer.[0..len-1]
            let mutable start = 0
            for i in 0..text.Length-1 do
                if text.[i..i+3] = "\";\r\n" then
                    let line = text.[start..i-1]
                    if line.[0] <> '"' then
                        failwithf "JDF line did not start with quote"
                    let line = line.[1..]
                    start <- i+4
                    
                    if not <| String.IsNullOrWhiteSpace(line) then
                        yield line.Split("\",\"") |> rowParser
            text <- text.[start..]
    }

let tryParseJdfTextFromZip parser (zip: ZipArchive) (filename: string) =
    zip.Entries
    |> Seq.tryFind (fun entry -> entry.Name.ToLower() = filename.ToLower())
    |> Option.map (fun entry ->
        parser <| entry.Open())
