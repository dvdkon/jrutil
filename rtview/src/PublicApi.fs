// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

namespace RtView

open System
open System.ComponentModel.DataAnnotations 
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc

open Npgsql
open JrUtil.SqlRecordStore
open RtView.ServerGlobals

[<ApiController; Route("api")>]
type PublicApiController() =
    inherit ControllerBase()
    // PostgreSQL doesn't support native parameters in COPY,
    // so we make do with good ol' hand-escaping. Gross.
    let escape (s: string) = s.Replace("'", "''")

    let sqlCsvContent query =
        let conn = getDbConn ()
        let reader =
            conn.BeginTextExport
                (sprintf "COPY (%s) TO STDOUT WITH (FORMAT csv, HEADER)" query)
        // In ASP.NET Core 6 MVC, this can't be async AFAIK
        // In newer versions we could use an IResult instead
        { new ActionResult() with
            member this.ExecuteResult(ctx) =
                let resp = ctx.HttpContext.Response
                resp.ContentType <- "text/csv; charset=UTF-8"

                let buf = Array.create (1024*1024) '\000'
                let mutable hasData = true;
                while hasData do
                    let len = reader.ReadBlock(buf, 0, buf.Length)
                    resp.WriteAsync(String buf.[0..len - 1]).Wait()
                    hasData <- len = buf.Length

                reader.Close()
                conn.Close()
                ()
        }

    [<HttpGet("stops")>]
    member this.stops([<Required>] stopIdLike) =
        sqlCsvContent <| sprintf """
            SELECT * FROM stops
            WHERE id LIKE '%s'
        """ (escape stopIdLike)

    [<HttpGet("tripDetails")>]
    member this.tripDetails(
            [<Required>] tripIdLike,
            [<Required>] fromDate,
            [<Required>] toDate) =
        sqlCsvContent <| sprintf """
            SELECT * FROM tripdetails
            WHERE tripId LIKE '%s'
              AND tripStartDate >= '%s'::date
              AND tripStartDate <= '%s'::date
        """ (escape tripIdLike) (escape fromDate) (escape toDate)

    [<HttpGet("stopHistory")>]
    member this.stopHistory(
            [<Required>] tripIdLike,
            [<Required>] fromDate,
            [<Required>] toDate) =
        sqlCsvContent <| sprintf """
            SELECT * FROM stophistory
            WHERE tripId LIKE '%s'
              AND tripStartDate >= '%s'::date
              AND tripStartDate <= '%s'::date
        """ (escape tripIdLike) (escape fromDate) (escape toDate)

    [<HttpGet("coordHistory")>]
    member this.coordHistory(
            [<Required>] tripIdLike,
            [<Required>] fromDate,
            [<Required>] toDate) =
        sqlCsvContent <| sprintf """
            SELECT * FROM coordhistory
            WHERE tripId LIKE '%s'
              AND tripStartDate >= '%s'::date
              AND tripStartDate <= '%s'::date
        """ (escape tripIdLike) (escape fromDate) (escape toDate)
