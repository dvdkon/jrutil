// This file is part of JrUnify and is licenced under the GNU AGPLv3 or later
// (c) 2020 David Koňařík

module JrUnify.Utils

open System.IO

open Npgsql
open Serilog

open JrUtil.SqlRecordStore

let cleanGtfsTables conn =
    let cleanSql = """
    TRUNCATE stopTimes, trips, calendar, calendarExceptions, routes,
             stops, agencies;
    """
    executeSql conn cleanSql []

let deduplicateRoutes conn =
    let scriptPath = __SOURCE_DIRECTORY__ + "/DeduplicateRoutes.sql"
    let script = File.ReadAllText(scriptPath)
    executeSql conn script []

let convertZones conn =
    let scriptPath = __SOURCE_DIRECTORY__ + "/ConvertZones.sql"
    let script = File.ReadAllText(scriptPath)
    executeSql conn script []

let handleErrors formatStr formatParams func =
    try
        func()
    with
    | :? PostgresException as e ->
        Log.Error(sprintf "Error while %s:\nSQL error:\n{SqlError}" formatStr,
                  Array.append formatParams [| box e.Message |])
    | e ->
        Log.Error(e, sprintf "Error while %s" formatStr, formatParams)
