// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.Dpmlj

open System.IO
open Npgsql

open JrUtil
open JrUtil.SqlRecordStore

open JrUnify.Utils

let loadCisStopList (conn: NpgsqlConnection) path =
    cleanAndSetSchema conn "cis_stops"
    executeSql conn """
        CREATE TABLE stops (name text PRIMARY KEY);
        """ []
    use writer = conn.BeginTextImport("COPY stops FROM STDIN (FORMAT CSV)")
    writer.Write(File.ReadAllText(path))

let processDpmljGtfs conn path =
    handleErrors "processing DPMLJ GTFS" [||] (fun () ->
        cleanAndSetSchema conn "dpmlj"
        Gtfs.sqlCreateGtfsTablesNoConstrs conn
        Gtfs.sqlLoadGtfsFeed conn path

        // XXX: Workaround. Remove when clean data is available
        executeSql conn """
            DELETE FROM stoptimes
            WHERE NOT EXISTS (SELECT FROM stops WHERE id = stopid)
        """ []

        deduplicateRoutes conn

        executeSql conn """
            UPDATE trips
            SET routeid =
                '-CISR-545'
                || lpad((SELECT shortname FROM routes
                         WHERE id = routeid), 3, '0')
                || '-0';

            UPDATE routes
            SET id = '-CISR-545' || lpad(shortname, 3, '0') || '-0';
        """ []

        Gtfs.sqlCreateGtfsConstrs conn

        convertZones conn
    )
