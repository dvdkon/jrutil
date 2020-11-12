// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.GeoData.SqlCzptt

open System.IO

open JrUtil.SqlRecordStore

let initTables conn =
    executeSql conn """
        CREATE TABLE czptt_stops_geodata (
            name text NOT NULL,
            sr70 text,
            lat float NOT NULL,
            lon float NOT NULL,
            source text NOT NULL
        );
        CREATE INDEX ON czptt_stops_geodata USING GIST (name gist_trgm_ops);
    """ []

// Updates geodata in CZPTT stops from supplied sources
let applyCzpttStopsGeodata conn gtfs_schema =
    let template = compileSqlTemplate (File.ReadAllText(__SOURCE_DIRECTORY__ + "/ApplyCzpttStopsGeodata.sql"))
    let sql = template [
        "gtfs", box gtfs_schema
    ]
    executeSql conn sql []
