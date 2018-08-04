module JrUtil.JdfCsvParser

open System.IO
open System.Text.RegularExpressions
open System.Text
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open System.Text
open System.Text
open System.Globalization

exception JdfCsvParseException of msg: string with
    override this.Message = this.msg

// Unfortunately needed due to reflection methods returning null
[<AllowNullLiteral>]
type CsvSpreadAttribute(len: int) =
    inherit Attribute()
    member this.Len = len

[<AllowNullLiteral>]
type CsvValueAttribute(value: string) =
    inherit Attribute()
    member this.Value = value

let getUnionCsvCases unionType =
    FSharpType.GetUnionCases(unionType)
    |> Array.collect (fun c ->
        c.GetCustomAttributes()
        |> Array.filter (fun a -> a :? CsvValueAttribute)
        |> Array.map (fun a -> a :?> CsvValueAttribute)
        |> Array.map (fun a -> (c, a.Value)))

let getUnionParser unionType =
    let cases = getUnionCsvCases unionType
    fun x ->
        let cases =
            cases
            |> Array.filter (fun (c, v) -> v = x)
            |> Array.map (fun (c, v) -> c)
        if cases.Length <> 1
        then raise (JdfCsvParseException
                     (sprintf "No union case for value %s" x))
        FSharpValue.MakeUnion(cases.[0], [||])

let rec getColParser (colType: Type) =
    // Ifs are probably better than a match expression here
    let parseMethod = colType.GetMethod("CsvParse")
    if parseMethod <> null then (fun x -> parseMethod.Invoke(null, [|x|]))
    else if colType = typeof<string> then box
    else if colType = typeof<int> then int >> box
    else if colType = typeof<float> then float >> box
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
        let innerType = colType.GetGenericArguments().[0]
        let innerTypeParser = getColParser innerType
        let optionCases = FSharpType.GetUnionCases(colType)
        let someCase = optionCases |> Array.find (fun c -> c.Name = "Some")
        let noneCase = optionCases |> Array.find (fun c -> c.Name = "None")
        (fun x -> (if x = ""
                   then FSharpValue.MakeUnion(noneCase, [||])
                   else let innerVal = innerTypeParser x
                        FSharpValue.MakeUnion(someCase, [|innerVal|])))
    else if FSharpType.IsUnion(colType) then getUnionParser colType
    else raise (JdfCsvParseException "Could not convert type from CSV")

let getRowParser<'r> =
    let recordType = typeof<'r>
    assert FSharpType.IsRecord(recordType)
    let fields = FSharpType.GetRecordFields(recordType)
    let spreadAttrs =
        [for f in fields -> f.GetCustomAttribute<CsvSpreadAttribute>()]
    // A wrapper of getColParser that deals with spread fields
    let getFieldParser (f: PropertyInfo) =
        let colType = f.PropertyType
        if colType.IsArray
        then let innerType  = colType.GetElementType()
             getColParser innerType
        else getColParser colType
    let colParsers =
        fields
        |> Array.map (fun f -> getFieldParser f)
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

        FSharpValue.MakeRecord(recordType, List.toArray props) |> unbox<'r>

let parseCsv<'r> text =
    // The spec says "quotes inside text don't need to be doubled",
    // which is really confusing from an escaping standpoint
    // I take that to mean that there is really no quotation, and that
    // "," itself is the field separator (excluding the start (") and
    // the end (";)) and that it's unescapable
    // This, however, means that this CSV-esque format can be parsed by regex!
    let lines = (new Regex(";\\r\\n")).Split(text)
    let colRegex = new Regex("\",\"")
    let rowParser = getRowParser<'r>
    lines
    |> Array.filter (fun line -> line <> "")
    |> Array.map (fun line ->
            // Strip off leading and trailing quote
            let strippedLine = line.Substring(1, line.Length - 2)
            colRegex.Split(strippedLine) |> rowParser)


let parseCsvFile<'r> inpath =
    // This is probably not ideal. However, the file should never be more
    // than a few megabytes in size, in which case this will be faster than
    // FSharp.Data's approach, which reads char by char
    let encoding = CodePagesEncodingProvider.Instance.GetEncoding(1250)
    let text = File.ReadAllText(inpath, encoding)
    parseCsv<'r> text
