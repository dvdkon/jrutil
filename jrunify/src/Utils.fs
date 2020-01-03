// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.Utils

open System.IO

open Npgsql

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

let handleErrors description func =
    try
        func()
    with
    | :? PostgresException as e ->
        printfn "Error while %s:\nSQL error:\n%s\n"
                 description e.Message
    | e ->
        printfn "Error while %s :\n%A" description e
