// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.JdfCsvParser

open System.IO
open System.Text.RegularExpressions
open System.Text
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open System.Globalization

open JrUtil.Utils

exception JdfCsvParseException of msg: string with
    override this.Message = this.msg

// Unfortunately needed due to reflection methods returning null
[<AllowNullLiteral>]
type CsvSpreadAttribute(len: int) =
    inherit Attribute()
    member this.Len = len

let rec getColParser (colType: Type) =
    // Ifs are probably better than a match expression here
    let parseMethod = colType.GetMethod("CsvParse")
    if parseMethod <> null then (fun x -> parseMethod.Invoke(null, [|x|]))
    else if colType = typeof<string> then box
    else if colType = typeof<int> then int >> box
    else if colType = typeof<float> then float >> box
    else if colType = typeof<decimal> then decimal >> box
    else if colType = typeof<bool> then
        fun x -> match x with
                  // An empty string meaning false isn't according to spec,
                  // but occurs in real-world data
                  | "0" | "" -> false
                  | "1" -> true
                  | _ -> raise (JdfCsvParseException
                                 (sprintf "Invalid value for bool: %s" x))
                  |> box
    else if colType = typeof<DateTime> then
        fun x -> let (success, res) =
                     DateTime.TryParseExact(
                         x, [|"ddMMyyyy"; "HHmm"|],
                         CultureInfo.InvariantCulture,
                         DateTimeStyles.None)
                 if not success
                 then raise (JdfCsvParseException
                              (sprintf "Invalid value for DateTime %s" x))
                 else res |> box

    else if colType.IsGenericType
            && colType.GetGenericTypeDefinition() = typedefof<_ option> then
        let optionCases = FSharpType.GetUnionCases(colType)
        let someCase = optionCases |> Array.find (fun c -> c.Name = "Some")
        let noneCase = optionCases |> Array.find (fun c -> c.Name = "None")
        let someCtor = FSharpValue.PreComputeUnionConstructor(someCase)
        let noneCtor = FSharpValue.PreComputeUnionConstructor(noneCase)

        let innerType = colType.GetGenericArguments().[0]
        let innerTypeParser = getColParser innerType
        (fun x -> (if x = ""
                   then noneCtor [||]
                   else let innerVal = innerTypeParser x
                        someCtor [|innerVal|]))
    else if FSharpType.IsUnion(colType) then getUnionParser colType
    else raise (JdfCsvParseException "Could not convert type from CSV")

let getRowParser<'r> =
    let recordType = typeof<'r>
    assert FSharpType.IsRecord(recordType)
    let fields = FSharpType.GetRecordFields(recordType)
    let spreadAttrs =
        [for f in fields -> f.GetCustomAttribute<CsvSpreadAttribute>()]
    // A wrapper of getColParser that deals with spread fields
    // and adds error handling
    let getFieldParser (f: PropertyInfo) =
        let colType = f.PropertyType
        let parser = if colType.IsArray
                     then let innerType  = colType.GetElementType()
                          getColParser innerType
                     else getColParser colType
        fun x ->
            try parser x
            with
            | _ as e ->
                raise (JdfCsvParseException
                        // Unfortunately, there's no way to have multiline
                        // format strings AFAIK
                        (sprintf ("Failed parsing field \"%s\" of value \"%s\": %s")
                                 f.Name x (e.ToString())))
    let colParsers =
        fields
        |> Array.map (fun f -> getFieldParser f)

    let recordConstructor = FSharpValue.PreComputeRecordConstructor(recordType)
    fun (cols: string array) ->
        let (_, _, props) =
            // This deals with the CsvSpread attribute. Sorry for the ugly code
            spreadAttrs
            |> List.fold (fun (ri, ci, l) sa ->
                match sa with
                // @ isn't as efficient as ::, but it's easier to do it this
                // way IMO (:: doesn't interact well with list comprehensions)
                | null -> (ri + 1, ci + 1, l @ [colParsers.[ri] cols.[ci]])
                | sa -> (ri + 1,
                         ci + sa.Len,
                         let vals = [|for i in ci..(ci + sa.Len - 1)
                                      -> colParsers.[ri] cols.[i]|]
                         let elType = fields.[ri].PropertyType.GetElementType()
                         let a = Array.CreateInstance(elType, vals.Length)
                         Array.Copy(vals, a, vals.Length)
                         l @ [a])
                )  (0, 0, [])

        recordConstructor(List.toArray props) |> unbox<'r>

let getCsvParser<'r> =
    // The spec says "quotes inside text don't need to be doubled",
    // which is really confusing from an escaping standpoint
    // I take that to mean that there is really no quotation, and that
    // "," itself is the field separator (excluding the start (") and
    // the end (";)) and that it's unescapable
    // This, however, means that this CSV-esque format can be parsed by regex!
    let rowParser = getRowParser<'r>
    fun text ->
        let lines = (new Regex(";\\r\\n")).Split(text)
        let colRegex = new Regex("\",\"")
        lines
        |> Array.filter (fun line -> line <> "")
        |> Array.map (fun line ->
                // Strip off leading and trailing quote
                let strippedLine = line.Substring(1, line.Length - 2)
                colRegex.Split(strippedLine) |> rowParser)


let getCsvFileParser<'r> =
    // This is probably not ideal. However, the file should never be more
    // than a few megabytes in size, in which case this will be faster than
    // FSharp.Data's approach, which reads char by char
    let parser = getCsvParser<'r>
    fun inpath ->
        let encoding = CodePagesEncodingProvider.Instance.GetEncoding(1250)
        let text = File.ReadAllText(inpath, encoding)
        parser text
