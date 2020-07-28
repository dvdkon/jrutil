// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.MergeAll

open Npgsql

open JrUtil
open JrUtil.GtfsMerge
open JrUtil.SqlRecordStore

open JrUnify.Utils

let mergeAll conn =
    cleanAndSetSchema conn "merged"
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        MergedFeed(conn, "merged", TripMergeStrategy.WithRoute, true)
    ["dpmlj"; "jdfbus_merged"; "jdfmhd_merged"; "czptt_merged"]
    |> List.iter (fun schema ->
        handleErrors "merging {Schema}" [| schema |] (fun () ->
            cleanAndSetSchema conn "merge_temp"
            mergedFeed.InsertFeed schema
        )
    )
