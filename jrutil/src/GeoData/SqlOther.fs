// This file is part of JrUtil and is licenced under the GNU GPLv2 or later
// (c) 2019 David Koňařík

module JrUtil.GeoData.SqlOther

open System.IO

open JrUtil.SqlRecordStore

let initTables conn =
    executeSql conn """
        CREATE TABLE other_stops_geodata (
            name text NOT NULL,
            lat float NOT NULL,
            lon float NOT NULL,
            source text NOT NULL
        );
        CREATE INDEX ON other_stops_geodata USING GIST (name gist_trgm_ops);
    """ []

let applyOtherStopsGeodata conn gtfsSchema =
    let template = compileSqlTemplate (File.ReadAllText(__SOURCE_DIRECTORY__ + "/ApplyOtherStopsGeodata.sql"))
    let sql = template [
        "gtfs", box gtfsSchema
    ]
    executeSql conn sql []
