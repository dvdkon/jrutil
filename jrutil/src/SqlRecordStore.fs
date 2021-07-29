// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.SqlRecordStore

open System
open System.Data.Common
open System.Collections.Generic
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open NodaTime
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

// Nowadays this module is pretty much PostgreSQL specific

type SqlRow(columnNames: string seq, cols: obj array) =
    member this.ColumnNames = columnNames
    member this.Cols = cols
    member this.Item(columnName) =
        match columnNames |> Seq.tryFindIndex ((=) columnName) with
        | None -> failwithf "Column \"%s\" not found. Columns: %A"
                            columnName columnNames
        | Some i -> cols.[i]
    member this.Item(columnIndex) =
        cols.[columnIndex]

NpgsqlConnection.GlobalTypeMapper.UseNodaTime() |> ignore

// Don't forget to .Close()/use the result!
let getPostgresqlConnection connstr =
    let conn = new NpgsqlConnection(connstr)
    conn.Open()
    conn.TypeMapper.UseNodaTime() |> ignore // TODO: Shouldn't be necessary
    conn

let rec sqlAdoTypeFor t =
    if t = typeof<string> then NpgsqlDbType.Text
    else if t = typeof<int> then NpgsqlDbType.Integer
    else if t = typeof<int64> then NpgsqlDbType.Bigint
    else if t = typeof<float32> then NpgsqlDbType.Real
    else if t = typeof<float> then NpgsqlDbType.Double
    else if t = typeof<decimal> then NpgsqlDbType.Numeric
    else if t = typeof<bool> then NpgsqlDbType.Boolean
    else if t = typeof<DateTime> then NpgsqlDbType.Timestamp
    else if t = typeof<LocalDateTime> then NpgsqlDbType.Timestamp
    else if t = typeof<Instant> then NpgsqlDbType.Timestamp
    else if t = typeof<LocalDate> then NpgsqlDbType.Date
    else if t = typeof<LocalTime> then NpgsqlDbType.Time
    else if t = typeof<Period> then NpgsqlDbType.Interval
    else if t.IsSubclassOf(typeof<DateTimeZone>) then NpgsqlDbType.Unknown
    else if t.IsSubclassOf(typeof<NpgsqlRange<_>>) then NpgsqlDbType.Range
    else if t = typeof<DBNull> then NpgsqlDbType.Unknown
    else if typeIsOption t then
        let innerType = t.GetGenericArguments().[0]
        sqlAdoTypeFor innerType
    else if FSharpType.IsUnion(t) then NpgsqlDbType.Text
    else if t.IsArray then
        let innerAdoType = sqlAdoTypeFor (t.GetElementType())
        NpgsqlDbType.Array ||| innerAdoType
    else failwithf "Could not get SQL parameter type for %A" t

let sqlTypeFor (type_: Type) =
    let rec sqlTypeForNonOptional t =
        if t = typeof<string> then "TEXT"
        else if t = typeof<int> then "INTEGER"
        else if t = typeof<int64> then "BIGINT"
        else if t = typeof<float32> then "FLOAT4"
        else if t = typeof<float> then "FLOAT8"
        else if t = typeof<decimal> then "DECIMAL"
        else if t = typeof<bool> then "BOOLEAN"
        else if t = typeof<DateTime> then "TIMESTAMP"
        else if t = typeof<LocalDateTime> then "TIMESTAMP"
        else if t = typeof<Instant> then "TIMESTAMP"
        else if t = typeof<LocalDate> then "DATE"
        else if t = typeof<LocalTime> then "TIME"
        else if t = typeof<Period> then "INTERVAL"
        else if t = typeof<DateTimeZone> then "TEXT"
        else if t = typeof<NpgsqlRange<LocalDate>> then "DATERANGE"
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

let rec createSqlValuePreparer t =
    if typeIsOption t then
        let innerPreparer = createSqlValuePreparer t.GenericTypeArguments.[0]
        let valueProp = t.GetProperty("Value")
        fun value ->
            if isNull value || optionCaseGetter value <> optionSomeCase.Tag
            then box DBNull.Value
            else innerPreparer (valueProp.GetValue(value))
    else
        fun value ->
            if isNull value then box DBNull.Value
            else box value

let getSqlValuePreparer = memoize createSqlValuePreparer

let sqlPrepareValue (value: obj) = getSqlValuePreparer (value.GetType()) value

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
        let param = NpgsqlParameter(name, adoType)
        param.Value <- value
        param)
    cmd.Parameters.AddRange(Seq.toArray sqlParams)
    cmd

let executeSql conn sql paramSeq =
    use cmd = createSqlCommand conn sql paramSeq
    cmd.ExecuteNonQuery() |> ignore // TODO: Is async desirable?

let sqlQuery conn sql paramSeq =
    use cmd = createSqlCommand conn sql paramSeq
    use reader = cmd.ExecuteReader()
    let colNames = reader.GetColumnSchema() |> Seq.map (fun c ->
        c.ColumnName
    )
    let rows = new List<SqlRow>()
    while reader.Read() do
        let cols = Array.zeroCreate reader.FieldCount
        reader.GetValues(cols) |> ignore
        rows.Add(SqlRow(colNames, cols))
    rows

let sqlQueryOne conn sql paramSeq =
    use cmd = createSqlCommand conn sql paramSeq
    cmd.ExecuteScalar()

let rec parseSqlValue tgtType (value: obj) =
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
    // By default SQL timestamp gets returned as Instant, but sometimes we want
    // LocalDateTime, which also maps to timestamp on insert
    else if tgtType = typeof<LocalDateTime>
         && value.GetType() = typeof<Instant> then
        box <| (value :?> Instant).InUtc().LocalDateTime
    // Timezones get inserted as strings
    else if tgtType = typeof<DateTimeZone> then
        assert (value.GetType() = typeof<string>)
        box <| DateTimeZoneProviders.Tzdb.[value :?> string]
    else value

let getSqlRecQuerier = memoize <| fun (recType: Type) ->
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
            if (results.[0].ColumnNames
                |> Seq.compareWith Operators.compare
                                   (fieldNames |> Seq.map sqlIdent)) <> 0
            then
                failwithf "SQL column/F# field name mismatch\nColumns: %A\nFields: %A"
                          (results.[0].ColumnNames |> Seq.toList) fieldNames
        results |> Seq.map (fun r ->
            let vals =
                r.Cols
                |> Seq.mapi (fun i v ->
                    parseSqlValue fieldTypes.[i] v)
                |> Seq.toArray
            constructor(vals))

let sqlQueryRec<'a> conn sql paramSeq =
    getSqlRecQuerier typeof<'a> conn sql paramSeq |> Seq.cast<'a>

let sqlInsertColumnsStr recType =
    FSharpType.GetRecordFields(recType)
    |> Seq.map (fun f -> sprintf "\"%s\"" (sqlIdent f.Name))
    |> String.concat ", "

let getSqlInserterTemplated = memoize <| fun recType ->
    assert FSharpType.IsRecord(recType)
    let fieldsGetter = FSharpValue.PreComputeRecordReader recType
    let columnsStr = sqlInsertColumnsStr recType
    let preparers =
        FSharpType.GetRecordFields(recType)
        |> Seq.map (fun f -> getSqlValuePreparer f.PropertyType)
        |> Seq.toArray
    fun template table (conn: DbConnection) (os: obj seq) ->
        if not <| Seq.isEmpty os then
            let ps =
                os
                |> Seq.mapi (fun i o ->
                    fieldsGetter o
                    |> Seq.mapi (fun j v ->
                        (sprintf "@%d_%d" i j, preparers.[j] v)))
                |> Seq.concat
            let paramsStr =
                seq { for i in 0 .. (Seq.length os) - 1 ->
                      seq { for j in 0 .. (Seq.length preparers) - 1 ->
                            sprintf "@%d_%d" i j }
                      |> String.concat ", "
                      |> fun s -> "(" + s + ")" }
                |> String.concat ", "
            let sql =
                template (sqlIdent table) columnsStr paramsStr
            executeSql conn sql ps

let getSqlInserter recType =
    getSqlInserterTemplated
        recType
        (sprintf """INSERT INTO "%s" (%s) VALUES %s""")

let sqlInsert<'a> conn table (os: 'a seq) =
    let inserter = getSqlInserter typeof<'a>
    inserter table conn (Seq.map box os)

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

let getSqlInCopier = memoize <| fun (recType: Type) ->
    // TODO: Merge with createSqlValuePreparer?
    let rec createCopyInValuePreparer t =
        if typeIsOption t then
            let innerPreparer =
                createCopyInValuePreparer t.GenericTypeArguments.[0]
            let valueProp = t.GetProperty("Value")
            fun value ->
                if isNull value || optionCaseGetter value <> optionSomeCase.Tag
                then box DBNull.Value
                else innerPreparer (valueProp.GetValue(value))
        elif FSharpType.IsUnion(t) then
            let serializer = getUnionSerializer t
            serializer >> box
        else
            fun value ->
                if isNull value then box DBNull.Value
                else box <| value
    let getCopyInValuePreparer = memoize createCopyInValuePreparer

    assert FSharpType.IsRecord(recType)
    let fieldsGetter = FSharpValue.PreComputeRecordReader recType
    let columnWriters =
        FSharpType.GetRecordFields(recType)
        |> Seq.mapi (fun i f ->
            let sqlType = sqlAdoTypeFor f.PropertyType
            let preparer = getCopyInValuePreparer f.PropertyType
            fun (writer: NpgsqlBinaryImporter) (data: obj array) ->
                let prepared = preparer data.[i]
                if prepared = box DBNull.Value then
                    writer.WriteNull()
                else
                    writer.Write<obj>(prepared, sqlType)
        )
    fun table (conn: NpgsqlConnection) (objs: obj array) ->
        let query = sprintf "COPY \"%s\" FROM STDIN (FORMAT BINARY)"
                            (sqlIdent table)
        use writer = conn.BeginBinaryImport(query)
        for o in objs do
            let fields = fieldsGetter o
            writer.StartRow()
            columnWriters |> Seq.iter (fun cw -> cw writer fields)
        writer.Complete()

// A wrapper over Scriban that replaces "#ident" with "{{ident}}"
// Note that this just does textual replacements, there's no escaping
// and SQLi protection
let compileSqlTemplate =
    let refRegex = Regex(@"(?<!\$)#([a-zA-Z_]+)(?=[^a-zA-Z_$])")
    fun sql ->
        let templateStr = refRegex.Replace(sql, @"{{$1}}")
        let template = Template.Parse(templateStr)
        if template.HasErrors then
            failwithf "Errors in SQL template!\n%s"
                      (template.Messages
                       |> Seq.map (fun x -> x.ToString())
                       |> String.concat "\n")
        fun (vars: (string * obj) seq) ->
            let context = TemplateContext()
            context.StrictVariables <- true
            let globals = ScriptObject()
            vars |> Seq.iter (fun (k, v) ->
                globals.Add(k, v)
            )
            context.PushGlobal(globals)
            template.Render(context)

let setSchema conn schemaName =
    executeSql conn (sprintf "SET search_path TO %s, public" schemaName) []

let cleanAndSetSchema conn schemaName =
    let sql =
        sprintf """
                DROP SCHEMA IF EXISTS %s CASCADE;
                CREATE SCHEMA IF NOT EXISTS %s;
                SET search_path TO %s, public;
                """
                schemaName schemaName schemaName
    executeSql conn sql []
