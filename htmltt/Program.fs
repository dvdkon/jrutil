// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module HtmlTt.Main

open System
open Docopt

open JrUtil.Utils
open JrUtil.SqlRecordStore

let docstring = (fun (s: string) -> s.Trim()) """
htmltt, a tool for generating timetables in HTML from GTFS

Usage:
    htmltt.exe generate <db-connstr> <out-dir>
    htmltt.exe serve <db-connstr> <bind-addr>
"""

[<EntryPoint>]
let main argv =
    withProcessedArgs docstring argv (fun args ->
        let dbConn = getPostgresqlConnection (argValue args "<db-connstr>")

        if argFlagSet args "generate" then
            printfn "Not implemented yet"
            dbConn.Close()
            2
        else if argFlagSet args "serve" then
            Server.runServer dbConn (argValue args "<bind-addr>")
            dbConn.Close()
            0
        else failwith "Unreachable"
    )
