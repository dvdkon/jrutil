// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CsvParser

open System

open System.Globalization
open System.Reflection
open Microsoft.FSharp.Reflection

open JrUtil.ReflectionUtils
open JrUtil.UnionCodec
open JrUtil.CsvMetadata

exception CsvParseException of msg: string
with override this.Message = this.msg

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
    else if colType = typeof<int64> then int64 >> box
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
    else if typeIsOption colType then
        let someCtor, noneCtor = getOptionConstructors colType
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
                        (sprintf "Failed parsing field \"%s\" of value \"%s\": %s"
                                 f.Name x (e.ToString())))
    let colParsers =
        fields |> Array.map getFieldParser
    let elTypes = fields |> Array.map (fun f ->
        f.PropertyType.GetElementType())

    let recordConstructor = FSharpValue.PreComputeRecordConstructor(recordType)
    fun (cols: string array) ->
        let props = Array.create colParsers.Length null
        let mutable ri = 0 // Record field index
        let mutable ci = 0 // Cell index
        for sa in spreadAttrs do
            match sa with
            | null ->
                props.[ri] <- colParsers.[ri] cols.[ci]
                ri <- ri + 1
                ci <- ci + 1
            | sa ->
                let a = Array.CreateInstance(elTypes.[ri], sa.Len)
                for i in 0..(sa.Len - 1) do
                    a.SetValue(colParsers.[ri] cols.[i + ci], i)
                props.[ri] <- a
                ri <- ri + 1
                ci <- ci + sa.Len
        recordConstructor(props) |> unbox<'r>
