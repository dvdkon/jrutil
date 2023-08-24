// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module RtView.Utils

open System.Web
open Microsoft.AspNetCore.Mvc
open NodaTime
open Giraffe.ViewEngine

open JrUtil.Utils
open JrUtil.SqlRecordStore
open RtView.ServerGlobals

let getTripName (tripId: string) (startDate: LocalDate) () =
    use c = getDbConn ()
    let name = sqlQueryOne c """
            SELECT COALESCE(shortName, @tripId)
            FROM tripDetails
            WHERE tripId = @tripId
              AND tripStartDate = @startDate::date
            LIMIT 1
            """ ["tripId", box tripId
                 "startDate", box startDate]
    name :?> string

let nullToNone x = if x = null then None else Some x

let dateRangeOrDefaults fromDateParam toDateParam =
    let today = dateToday ()
    let weekAgo = today.PlusWeeks(-1)
    let orDefault d v = if v = LocalDate() then d else v
    fromDateParam |> orDefault weekAgo,
    toDateParam |> orDefault today

let dateRangeControl
        (fromDate: LocalDate) (toDate: LocalDate) =
    div [_class "date-range"] [
        label [] [
            str "From:"
            input [_type "date"
                   _name "fromDate"
                   _value <| dateToIso fromDate]
        ]
        label [] [
            str "To:"
            input [_type "date"
                   _name "toDate"
                   _value <| dateToIso toDate]
        ]
        button [] [str "Set"]
    ]

let empty = str ""

let pageMaster t content =
    html [_lang "en"] [
        head [] [
            title [] [str $"RtView: {t}"]
            meta [_charset "utf-8"]
            meta [_name "viewport"
                  _content "width=device-width, initial-scale=1.0"]
            script [_src "/rtview.js"; _defer] []
            script [_src "/chart.umd.js"; _defer] []
            link [_rel "stylesheet"
                  _type "text/css"
                  _href "/rtview.css"]
        ]
        body [] [
            content
        ]
    ]

let htmlResult title html =
    let text =
        pageMaster title html
        |> RenderView.AsString.htmlDocument
    ContentResult(Content = text, ContentType = "text/html")

let htmlFragmentResult html =
    let text = RenderView.AsString.htmlNode html
    ContentResult(Content = text, ContentType = "text/html")

module Links =
    let routes fromDate toDate (search: string) page =
        sprintf "/Routes?fromDate=%s&toDate=%s&search=%s&page=%d"
                (dateToIso fromDate) (dateToIso toDate)
                (HttpUtility.UrlEncode(search))
                page
    let trips id fromDate toDate =
        sprintf "/Trips/%s?fromDate=%s&toDate=%s"
                id (dateToIso fromDate) (dateToIso toDate)
    let trip id startDate =
        sprintf "/Trip/%s/%s" id (dateToIso startDate)
