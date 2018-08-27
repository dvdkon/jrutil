// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.CsvParser

open System

open JrUtil.Utils
open System.Globalization
open Microsoft.FSharp.Reflection
open System.Reflection

exception CsvParseException of msg: string with
    override this.Message = this.msg

// Unfortunately needed due to reflection methods returning null
[<AllowNullLiteral>]
type CsvSpreadAttribute(len: int) =
    inherit Attribute()
    member this.Len = len

let dateTimeParser (formats: string array) (instr: string) =
    let (success, res) =
        DateTime.TryParseExact(
            instr, formats,
            CultureInfo.InvariantCulture,
            DateTimeStyles.None)
    if not success
    then raise (CsvParseException
                 (sprintf "Invalid value for DateTime: %s" instr))
    else res |> box

let rec colParserForBase colParserFor (colType: Type) =
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
                  | _ -> raise (CsvParseException
                                 (sprintf "Invalid value for bool: %s" x))
                  |> box
    else if colType.IsGenericType
            && colType.GetGenericTypeDefinition() = typedefof<_ option> then
        let optionCases = FSharpType.GetUnionCases(colType)
        let someCase = optionCases |> Array.find (fun c -> c.Name = "Some")
        let noneCase = optionCases |> Array.find (fun c -> c.Name = "None")
        let someCtor = FSharpValue.PreComputeUnionConstructor(someCase)
        let noneCtor = FSharpValue.PreComputeUnionConstructor(noneCase)

        let innerType = colType.GetGenericArguments().[0]
        let innerTypeParser = colParserFor innerType
        (fun x -> (if x = ""
                   then noneCtor [||]
                   else let innerVal = innerTypeParser x
                        someCtor [|innerVal|]))
    else if FSharpType.IsUnion(colType) then getUnionParser colType
    else raise (CsvParseException
                 (sprintf "Could not convert type from CSV: %A" colType))

let getRowParser<'r> (colParserFor: Type -> (string -> obj)) =
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
                          colParserFor innerType
                     else colParserFor colType
        fun x ->
            try parser x
            with
            | _ as e ->
                raise (CsvParseException
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
