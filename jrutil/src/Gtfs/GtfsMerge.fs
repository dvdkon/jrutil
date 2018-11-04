// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsMerge

open System.IO
open System.Data.Common
open System.Text.RegularExpressions
open Scriban
open Scriban.Runtime

open JrUtil.GtfsModel
open JrUtil.SqlRecordStore

// A wrapper over Scriban that replaces "$ident" with "{{ident}}"
let compileSqlTemplate =
    let refRegex = new Regex(@"(?<!\$)\$([a-zA-Z_]+)(?=[^a-zA-Z_$])")
    fun sql ->
        let templateStr = refRegex.Replace(sql, @"{{$1}}")
        let t = Template.Parse(templateStr)
        if t.HasErrors then
            failwithf "Errors in SQL template!\n%s"
                      (t.Messages
                       |> Seq.map (fun x -> x.ToString())
                       |> String.concat "\n")
        t

type MergedFeed(conn: DbConnection,
                schema: string,
                ?checkStopType: bool,
                ?checkStations: bool) =
    let checkStopType = defaultArg checkStopType true
    let checkStations = defaultArg checkStations false

    // TODO: Include template at compile-time? This is really fragile
    static let template = compileSqlTemplate (File.ReadAllText(__SOURCE_DIRECTORY__ + "/GtfsMerge.sql"))

    let mutable feedNum = 0

    member this.InsertFeed feedSchema =
        feedNum <- feedNum + 1

        let context = new TemplateContext()
        context.StrictVariables <- true
        let vars = new ScriptObject()
        vars.Add("merged", schema)
        vars.Add("in", feedSchema)
        vars.Add("feednum", feedNum)
        vars.Add("check_stations", checkStations)
        context.PushGlobal(vars)
        let sql = template.Render(context)
        executeSql conn sql []

    member this.ToGtfsFeed() =
        failwith "Not implemented yet"
