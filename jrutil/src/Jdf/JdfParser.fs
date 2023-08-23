// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfParser

open System.IO
open System.IO.Compression
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


let jdfEncoding = CodePagesEncodingProvider.Instance.GetEncoding(1250)

let readJdfTextFromFile inpath =
    // This is probably not ideal. However, the file should never be more
    // than a few megabytes in size, in which case this will be faster than
    // FSharp.Data's approach, which reads char by char
    File.ReadAllText(inpath, jdfEncoding)

let tryReadJdfTextFromZip zipPath (filename: string) =
    ZipFile.OpenRead(zipPath).Entries
    |> Seq.tryFind (fun entry -> entry.Name.ToLower() = filename.ToLower())
    |> Option.map (fun entry ->
        use reader = new StreamReader(entry.Open(), jdfEncoding)
        reader.ReadToEnd()
    )

/// path: Path to JDF batch (folder or .zip)
/// name: filename inside batch (case-insensitive)jj
let tryReadJdfText (path: string) name =
    let ext = Path.GetExtension(path)
    if Directory.Exists(path) then
        findPathCaseInsensitive path name
        |> Option.map readJdfTextFromFile
    else if ext.ToLower() = ".zip" then
        tryReadJdfTextFromZip path name
    else None
