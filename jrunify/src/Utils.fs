// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.Utils

open System.IO

open JrUtil.SqlRecordStore

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

let cleanGtfsTables conn =
    let cleanSql = """
    DELETE FROM stopTimes;
    DELETE FROM trips;
    DELETE FROM calendar;
    DELETE FROM calendarExceptions;
    DELETE FROM routes;
    DELETE FROM stops;
    DELETE FROM agencies;
    """
    executeSql conn cleanSql []

let normaliseStopNames conn stopsTable prefix =
    let scriptPath = __SOURCE_DIRECTORY__ + "/NormaliseStopNames.sql"
    let script = File.ReadAllText(scriptPath)
    let template = compileSqlTemplate script
    let sql = template ["tgtstops", stopsTable]
    executeSql conn sql [
        "prefix", box prefix
        "threshold", box 0.7]
    ()

let deduplicateRoutes conn =
    let scriptPath = __SOURCE_DIRECTORY__ + "/DeduplicateRoutes.sql"
    let script = File.ReadAllText(scriptPath)
    executeSql conn script []
