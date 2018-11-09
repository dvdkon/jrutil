// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.GtfsMerge

open System.IO
open System.Data.Common
open System.Text.RegularExpressions

open JrUtil.GtfsModel
open JrUtil.SqlRecordStore

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
        let sql = template [
            "merged", box schema;
            "in", box feedSchema;
            "feednum", box feedNum;
            "check_stations", box checkStations;
        ]
        executeSql conn sql []

    member this.ToGtfsFeed() =
        failwith "Not implemented yet"
