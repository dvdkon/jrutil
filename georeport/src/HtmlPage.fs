// This file is part of JrUtil and is licenced under the GNU GPLv2 or later
// (c) 2019 David Koňařík

module GeoReport.HtmlPage

open Giraffe.GiraffeViewEngine

open GeoReport.Processing

let osmMapUrl = "https://www.openstreetmap.org"

let globalStyle = """
* {
    font-family: sans-serif;
}

table {
    border: 2px solid slateblue;
    border-collapse: collapse;
}

table tr {
    border: 1px solid lightsteelblue;
}

table td {
    padding: 2px 5px 2px 5px;
}

ul {
    margin: 0;
}

a {
    color: #356688;
    text-decoration: none;
}

.matches-by-type {
    margin-bottom: 10px;
}

.more-matches {
    border: 1px solid #aaa;
    background-color: #eee;
    border-radius: 2px;
}

.match-none {
    background-color: #ffb6ce;
}

.match-osm-tag {
    background-color: #a6ffce;
}

.match-osm-name {
    background-color: #8becd5;
}

.match-external {
    background-color: #fbe1a5;
}

.stddev-0 {
    background-color: #e1fccc;
}

.stddev-1 {
    background-color: #ecfc76;
}

.stddev-2 {
    background-color: #fce376;
}

.stddev-3 {
    background-color: #fc769a;
}
"""

let globalScript = """
function toggleDetails(btn) {
    var detailsRow = btn.parentElement.parentElement.nextSibling;
    if(detailsRow.style.display === "none") {
        detailsRow.style.display = "";
        btn.innerText = "Less";
    } else {
        detailsRow.style.display = "none";
        btn.innerText = "More";
    }
}
"""

// Copied from HtmlTt, TODO create common module?
let table2 cols content =
    table [] [
        thead [] [
            tr [] [
                for col in cols ->
                th [] [str col]
            ]
        ]
        tbody [] content
    ]

let stopTable stopMatches =
    let matchLink ms =
        let latAvg = ms |> Seq.map (fun m -> m.lat) |> Seq.average
        let lonAvg = ms |> Seq.map (fun m -> m.lon) |> Seq.average
        a [_href (sprintf "%s?mlat=%f&mlon=%f#map=16/%f/%f" osmMapUrl
                          latAvg lonAvg latAvg lonAvg)]

    let topMatchInfo includeSrcName tm =
        let tmopt = Option.map (fun m -> (Seq.head m).matchType) tm
        match tmopt with
        | Some (OsmMatchByTag _) -> "match-osm-tag", "OSM by tag"
        | Some (OsmMatchByName _) -> "match-osm-name", "OSM by name"
        | Some (ExternalSource src) ->
            "match-external",
            if includeSrcName
            then sprintf "External source: %s" src
            else "External source"
        | None -> "match-none", "No match"

    let matchShort matches =
        let top = topMatches matches
        let (cls, desc) = topMatchInfo true top
        td [_class cls]
            (top
             |> Option.map (fun top -> [matchLink top [str desc]])
             |> Option.defaultValue [str "No match"])

    let matchDetail m =
        match m.matchType with
        | OsmMatchByTag osmid ->
            a [_href (sprintf "%s/node/%d" osmMapUrl osmid)]
              [str "OSM by tag"]
        | OsmMatchByName osmid ->
            a [_href (sprintf "%s/node/%d" osmMapUrl osmid)]
              [str "OSM by name"]
        | ExternalSource src ->
            matchLink [m] [str (sprintf "External source: %s" src)]

    // TODO: Maybe compute only from top matches?
    let matchStdDev ms =
        let stdDev xs =
            let avg = Seq.average xs
            sqrt ((xs |> Seq.map (fun x -> (x - avg)**2.0) |> Seq.sum)
                  / (Seq.length xs |> float))
        let lats = ms |> Seq.map (fun m -> m.lat)
        let lons = ms |> Seq.map (fun m -> m.lon)
        // We need just one value to report
        (((stdDev lats) + (stdDev lons))/2.0)*60.0

    let stdDevClass sd =
        if sd < 0.1 then "stddev-0"
        else if sd < 0.5 then "stddev-1"
        else if sd < 1.0 then "stddev-2"
        else "stddev-3"

    let topMatchTypes =
        stopMatches
        |> Seq.map (fun (_, ms) -> topMatches ms)
        |> Seq.groupBy (topMatchInfo false)
        |> Seq.map (fun (mt, ms) ->
            mt, (float <| Seq.length ms) / (float <| Seq.length stopMatches))
        |> Seq.sortBy (fun (mt, c) -> -c)

    div [] [
        h3 [] [str "Best match by type"]
        table [_class "matches-by-type"] [
            for ((cls, desc), count) in topMatchTypes ->
            tr [_class cls] [
                td [] [div [] [str desc]]
                td [] [str (sprintf "%f%%" (count*100.0))]
            ]
        ]

        table2 ["Name"; "Best match"; "Std. dev."; ""] [
            for (name, matches) in stopMatches do
            let stddev = match Seq.length matches with
                         | 0 -> None
                         | _ -> Some (matchStdDev matches)
            yield! [
                tr [_class "stop"] [
                    td [] [str name]
                    matchShort matches
                    match stddev with
                    | Some sd ->
                        td [_class (stdDevClass sd)]
                           [str <| sprintf "%.2f'" sd]
                    | None -> td [] []
                    td [] [
                        button [_class "more-matches"
                                _onclick "toggleDetails(this)"]
                               [str "More"]
                    ]
                ]
                tr [_class "stop-matches-detail"; _style "display: none"] [
                    td [_colspan "3"] [
                        ul [_class "match-list"] [
                            for m in matches ->
                            li [] [matchDetail m]
                        ]
                    ]
                ]
            ]
        ]
    ]

let resultPage railMatches otherMatches =
    html [] [
        head [] [
            title [] [str "JrUtil GeoReport results"]
            meta [_charset "UTF-8"]
            style [] [rawText globalStyle]
            script [] [rawText globalScript]
        ]
        body [] [
            h1 [] [str "Railway stops"]
            stopTable railMatches
        ]

        body [] [
            h1 [] [str "Other stops"]
            stopTable otherMatches
        ]

        footer [] [
            str "Generated by"
            a [_href "https://gitlab.com/dvdkon/jrutil/tree/master/georeport"]
              [str "JrUtil GeoReport"]
        ]
    ]

