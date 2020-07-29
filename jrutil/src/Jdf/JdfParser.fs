// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.JdfParser

open System.IO
open System.Text.RegularExpressions
open System.Text
open NodaTime

open JrUtil.Utils
open JrUtil.CsvParser

let rec jdfColParserFor colType =
    if colType = typeof<LocalDate> then
        parseDate "ddMMyyyy" >> box
    else if colType = typeof<LocalTime> then
        parseTime "HHmm" >> box
    else
        colParserForBase jdfColParserFor colType


let getJdfParser<'r> =
    // The spec says "quotes inside text don't need to be doubled",
    // which is really confusing from an escaping standpoint
    // I take that to mean that there is really no quotation, and that
    // "," itself is the field separator (excluding the start (") and
    // the end (";)) and that it's unescapable
    // This, however, means that this CSV-esque format can be parsed by regex!
    let rowParser = getRowParser<'r> jdfColParserFor
    fun text ->
        let lines = Regex(";\\r\\n").Split(text)
        let colRegex = Regex("\",\"")
        lines
        |> Array.filter (fun line -> line <> "")
        |> Array.map (fun line ->
                // Strip off leading and trailing quote
                let strippedLine = line.Substring(1, line.Length - 2)
                colRegex.Split(strippedLine) |> rowParser)


let getJdfFileParser<'r> =
    // This is probably not ideal. However, the file should never be more
    // than a few megabytes in size, in which case this will be faster than
    // FSharp.Data's approach, which reads char by char
    let parser = getJdfParser<'r>
    fun inpath ->
        let encoding = CodePagesEncodingProvider.Instance.GetEncoding(1250)
        let text = File.ReadAllText(inpath, encoding)
        parser text
