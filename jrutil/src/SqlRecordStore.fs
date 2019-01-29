// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.SqlRecordStore

open System
open System.Data
open System.Data.Common
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Npgsql
open NpgsqlTypes
open Scriban
open Scriban.Runtime

open JrUtil.Utils
open JrUtil.UnionCodec
open JrUtil.ReflectionUtils

// WARNING: This module does NOT deal with SQL injection in table and
// column names. It just relies on code being sane and no type/field having
// unwanted characters in their names.

// Most of the code is trying to be somewhat database portable, but there are
// places where that's just not possible and being specific to one database
// (PostgreSQL in this case) is inevitable. Other parts of the project are
// specific to PostgreSQL, so DB portability isn't really a priority.

type SqlRow(columnNames: string seq, cols: obj array) =
    member this.ColumnNames = columnNames
    member this.Cols = cols
    member this.Item(columnName) =
        let i = columnNames |> Seq.findIndex ((=) columnName)
        cols.[i]
    member this.Item(columnIndex) =
        cols.[columnIndex]

let getPostgresqlConnection connstr =
    new NpgsqlConnection(connstr)

let rec sqlAdoTypeFor t =
    if t = typeof<string> then NpgsqlDbType.Text
    else if t = typeof<int> then NpgsqlDbType.Integer
    else if t = typeof<float> then NpgsqlDbType.Real
    else if t = typeof<decimal> then NpgsqlDbType.Numeric
    else if t = typeof<bool> then NpgsqlDbType.Boolean
    else if t = typeof<DateTime> then NpgsqlDbType.Timestamp
    else if t = typeof<Date> then NpgsqlDbType.Unknown
    else if t = typeof<Time> then NpgsqlDbType.Unknown
    else if t = typeof<TimeSpan> then NpgsqlDbType.Interval
    else if t = typeof<DBNull> then NpgsqlDbType.Unknown
    else if FSharpType.IsUnion(t) then NpgsqlDbType.Text
    else if t.IsArray then
        let innerAdoType = sqlAdoTypeFor (t.GetElementType())
        NpgsqlDbType.Array ||| innerAdoType
    else failwithf "Could not get SQL parameter type for %A" t

let rec sqlPrepareValue value =
    if value = null then box DBNull.Value
    else
        let t = value.GetType()
        if typeIsOption t then
            sqlPrepareValue <| t.GetProperty("Value").GetValue(value)
        else if FSharpType.IsUnion(t) then getUnionSerializer t value |> box
        else box value

/// Takes an identifier and makes it lowercase, so that it can be
/// used in quotes by automatic scripts and without quotes in standard
/// hand-written SQL.
let sqlIdent (str: string) = str.ToLower()

let createSqlCommand (conn: DbConnection) sql (paramSeq: (string * obj) seq) =
    let cmd = conn.CreateCommand()
    cmd.CommandText <- sql
    let sqlParams = paramSeq |> Seq.map (fun (name, value) ->
        let value = sqlPrepareValue value
        let adoType = sqlAdoTypeFor (value.GetType())
        let param = new NpgsqlParameter(name, adoType)
        param.Value <- value
        param)
    cmd.Parameters.AddRange(Seq.toArray sqlParams)
    cmd

let executeSql conn sql paramSeq =
    let cmd = createSqlCommand conn sql paramSeq
    cmd.ExecuteNonQuery() |> ignore // TODO: Is async desirable?

let sqlQuery conn sql paramSeq =
    let cmd = createSqlCommand conn sql paramSeq
    let reader = cmd.ExecuteReader()
    let colNames = reader.GetColumnSchema() |> Seq.map (fun c ->
        c.ColumnName
    )
    let rows = new List<SqlRow>()
    while reader.Read() do
        let cols = Array.zeroCreate reader.FieldCount
        reader.GetValues(cols) |> ignore
        rows.Add(new SqlRow(colNames, cols))
    reader.Close()
    rows

let sqlQueryOne conn sql paramSeq =
    let cmd = createSqlCommand conn sql paramSeq
    cmd.ExecuteScalar()

let createSqlInserter = memoize <| fun recType ->
    assert FSharpType.IsRecord(recType)
    let fieldsGetter = FSharpValue.PreComputeRecordReader recType
    fun table (conn: DbConnection) (o: obj) ->
        let fields = fieldsGetter o
        let ps =
            [ for (i, v) in fields |> Seq.indexed -> (sprintf "@%d" i, v) ]
        let paramsStr =
            ps |> Seq.map (fun (n, _) -> n) |> String.concat ", "
        let sql = sprintf """INSERT INTO "%s" VALUES (%s)"""
                          (sqlIdent table) paramsStr
        executeSql conn sql ps

let sqlInsert conn table o =
    let inserter = createSqlInserter (o.GetType())
    inserter table conn o

let rec parseSqlValue tgtType value =
    if typeIsOption tgtType then
        let someCtor, noneCtor = getOptionConstructors tgtType
        if value.GetType() = typeof<DBNull> then
            noneCtor [||]
        else
            let innerType = tgtType.GetGenericArguments().[0]
            someCtor [| parseSqlValue innerType value |]
    else if FSharpType.IsUnion(tgtType) then
        assert (value.GetType() = typeof<string>)
        getUnionParser tgtType (unbox value)
    else value

let createSqlRecQuerier = memoize <| fun (recType: Type) ->
    assert FSharpType.IsRecord(recType)
    let constructor = FSharpValue.PreComputeRecordConstructor(recType)
    let fields = FSharpType.GetRecordFields(recType)
    let fieldNames = fields |> Array.map (fun f -> f.Name)
    let fieldTypes = fields |> Array.map (fun f -> f.PropertyType)
    fun (conn: DbConnection) sql (paramSeq: (string * obj) seq) ->
        let results = sqlQuery conn sql paramSeq
        if results.Count > 0 then
            // This is not strictly necessary, but helps catch bugs
            // If it proves to be too restrictive it can be removed
            assert ((results.[0].ColumnNames
                     |> Seq.compareWith Operators.compare fieldNames) = 0)
        results |> Seq.map (fun r ->
            let vals =
                r.Cols
                |> Seq.mapi (fun i v ->
                    parseSqlValue fieldTypes.[i] v)
                |> Seq.toArray
            constructor(vals))

let sqlQueryRec<'a> conn sql paramSeq =
    createSqlRecQuerier typeof<'a> conn sql paramSeq |> Seq.cast<'a>

let sqlTypeFor (type_: Type) =
    let rec sqlTypeForNonOptional t =
        if t = typeof<string> then "TEXT"
        else if t = typeof<int> then "INTEGER"
        else if t = typeof<float> then "FLOAT"
        else if t = typeof<decimal> then "DECIMAL"
        else if t = typeof<bool> then "BOOLEAN"
        else if t = typeof<DateTime> then "TIMESTAMP"
        else if t = typeof<Date> then "DATE"
        else if t = typeof<Time> then "TIME"
        else if t = typeof<TimeSpan> then "INTERVAL"
        // Sometimes non-nullablity can't be expressed in SQL, so just
        // give up and use the inner type directly.
        else if typeIsOption t then
            let innerType = t.GetGenericArguments().[0]
            sqlTypeForNonOptional innerType
        else if FSharpType.IsUnion(t) then "TEXT"
        else if t.IsArray then
            let innerType = t.GetElementType()
            sprintf "%s[]" (sqlTypeForNonOptional innerType)
        else failwithf "Could not get SQL type for %A" t

    let (t, isOption) =
        if typeIsOption type_ then
            let innerType = type_.GetGenericArguments().[0]
            (innerType, true)
        else (type_, false)

    (sqlTypeForNonOptional t) + if isOption then "" else " NOT NULL"

let recordSqlColsNullable recType =
    FSharpType.GetRecordFields(recType)
    |> Array.map (fun f -> typeIsOption f.PropertyType)

let createTableFor conn recType tableName =
    assert FSharpType.IsRecord(recType)
    let columns =
        FSharpType.GetRecordFields(recType)
        |> Seq.map (fun f ->
            sprintf "\"%s\" %s" (sqlIdent f.Name) (sqlTypeFor f.PropertyType)
        )
    let tableDecl =
        sprintf "CREATE TABLE \"%s\" (\n%s\n);"
            (sqlIdent tableName)
            (String.Join(",\n", columns))
    executeSql conn tableDecl []

let sqlCopyInText (conn: NpgsqlConnection)
                  table
                  (colsNullable: bool array)
                  data =
    let query =
        sprintf "COPY \"%s\" FROM STDIN (FORMAT text)"
                (sqlIdent table)
    use writer = conn.BeginTextImport(query)
    data |> Seq.iter (fun row ->
        let rowStr =
            row
            |> Array.mapi (fun i (col: string) ->
                if col = "" && colsNullable.[i]
                then @"\N"
                else col.Replace(@"\", @"\\")
                        .Replace("\n", @"\n")
                        .Replace("\t", @"\t"))
            |> String.concat "\t"
        writer.Write(rowStr + "\n");
    )

// A wrapper over Scriban that replaces "$ident" with "{{ident}}"
// Note that this just does textual replacements, there's no escaping
// and SQLi protection
let compileSqlTemplate =
    let refRegex = new Regex(@"(?<!\$)\$([a-zA-Z_]+)(?=[^a-zA-Z_$])")
    fun sql ->
        let templateStr = refRegex.Replace(sql, @"{{$1}}")
        let template = Template.Parse(templateStr)
        if template.HasErrors then
            failwithf "Errors in SQL template!\n%s"
                      (template.Messages
                       |> Seq.map (fun x -> x.ToString())
                       |> String.concat "\n")
        fun (vars: (string * obj) seq) ->
            let context = new TemplateContext()
            context.StrictVariables <- true
            let globals = new ScriptObject()
            vars |> Seq.iter (fun (k, v) ->
                globals.Add(k, v)
            )
            context.PushGlobal(globals)
            template.Render(context)
