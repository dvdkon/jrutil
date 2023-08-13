// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2020 David Koňařík

namespace RtView

open WebSharper

[<JavaScript>]
module ClientMain =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Client
    open WebSharper.Sitelets

    open RtView.ClientGlobals

    let pageWithLoadingIndicator doc =
        Doc.Concat [
            loadingTasks.View.Doc (fun lt ->
                if lt |> List.length > 0 then
                    div [attr.``class`` "loading-indicator"] [
                        yield div [attr.``class`` "heading"] [text "Loading..."]
                        for task in lt -> div [] [text task.description]
                    ]
                else Doc.Empty
            )
            doc
        ]

    [<SPAEntryPoint>]
    let clientMain () =
        router <- Router.Infer<Locations> ()
        location <- Router.Install Homepage router
        let pageDoc =
            location.View.Doc (function
                | Homepage -> RtView.Routes.Client.tripList None None None ()
                | Routes (f, t, s) -> RtView.Routes.Client.tripList f t s ()
                | Trips (tid, f, t) -> RtView.Trips.Client.tripsPage tid f t ()
                | Trip (tid, d) -> RtView.Trip.Client.tripPage tid d ()
            )
        pageWithLoadingIndicator pageDoc
        |> Doc.RunReplaceById "main"

module PublicApi =
    open Npgsql
    open WebSharper.Sitelets
    open JrUtil.SqlRecordStore
    open RtView.ServerGlobals

    type Endpoints =
        | [<EndPoint "/api/stops"; Query("stopIdLike")>]
          Stops of stopIdLike: string
        | [<EndPoint "/api/tripdetails";
            Query("tripIdLike", "fromDate", "toDate")>]
          TripDetails of tripIdLike: string
                       * fromDate: string
                       * toDate: string
        | [<EndPoint "/api/stophistory";
            Query("tripIdLike", "fromDate", "toDate")>]
          StopHistory of tripIdLike: string
                       * fromDate: string
                       * toDate: string
        | [<EndPoint "/api/coordhistory";
            Query("tripIdLike", "fromDate", "toDate")>]
          CoordHistory of tripIdLike: string
                        * fromDate: string
                        * toDate: string
        | [<EndPoint "/"; Wildcard>]
          CatchAll of string

    [<Website>]
    let website =
        // PostgreSQL doesn't support native parameters in COPY,
        // so we make do with good ol' hand-escaping. Gross.
        let escape (s: string) = s.Replace("'", "''")
        let sqlCsvContent query =
            let conn = getDbConn ()
            try
                let reader =
                    conn.BeginTextExport
                        (sprintf "COPY (%s) TO STDOUT WITH (FORMAT csv, HEADER)" query)
                Content.Custom(
                    Status = Http.Status.Ok,
                    Headers = [Http.Header.Custom "Content-Type" "text/csv; charset=UTF-8"],
                    WriteBody = fun outStream ->
                        let buf = Array.create 4096 '\000'
                        let mutable hasData = true;
                        while hasData do
                            let len = reader.ReadBlock(buf, 0, buf.Length)
                            outStream.Write(
                                System.ReadOnlySpan<byte>(
                                    System.Text.Encoding.UTF8.GetBytes(buf.[0..len - 1])))
                            hasData <- len = buf.Length

                        reader.Close()
                        conn.Close()
                )
            with
            | :? PostgresException as e ->
                conn.Close()
                Content.Text ("SQL error: " + e.MessageText)
                |> Content.SetStatus Http.Status.InternalServerError

        Sitelet.Infer <| fun ctx -> function
            | Stops stopIdLike ->
                sqlCsvContent <| sprintf """
                    SELECT * FROM stops
                    WHERE id LIKE '%s'
                """ (escape stopIdLike)
            | TripDetails (tripIdLike, fromDate, toDate) ->
                sqlCsvContent <| sprintf """
                    SELECT * FROM tripdetails
                    WHERE tripId LIKE '%s'
                      AND tripStartDate >= '%s'::date
                      AND tripStartDate <= '%s'::date
                """ (escape tripIdLike) (escape fromDate) (escape toDate)
            | StopHistory (tripIdLike, fromDate, toDate) ->
                sqlCsvContent <| sprintf """
                    SELECT * FROM stophistory
                    WHERE tripId LIKE '%s'
                      AND tripStartDate >= '%s'::date
                      AND tripStartDate <= '%s'::date
                """ (escape tripIdLike) (escape fromDate) (escape toDate)
            | CoordHistory (tripIdLike, fromDate, toDate) ->
                sqlCsvContent <| sprintf """
                    SELECT * FROM coordhistory
                    WHERE tripId LIKE '%s'
                      AND tripStartDate >= '%s'::date
                      AND tripStartDate <= '%s'::date
                """ (escape tripIdLike) (escape fromDate) (escape toDate)
            | CatchAll _ -> Content.File("wwwroot/index.html", ContentType="text/html; charset=UTF-8")
