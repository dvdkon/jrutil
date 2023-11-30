// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module GeoReport.StopListGen

open Serilog
open Serilog.Context

open JrUtil

let jdfStopNames czSkOnly jdfDir =
    let stopsParser = Jdf.fileParser "Zastavky.txt"

    Jdf.findJdfBatches jdfDir
    |> Seq.toArray
    |> Array.map (fun (batchName, batchPath) ->
        use _logCtx = LogContext.PushProperty("JdfBatch", batchName)
        Log.Information("Reading stops from {JDF}", batchName)
        try
            let stops: JdfModel.Stop array = stopsParser batchPath
            stops
            |> Array.filter (fun s ->
                not czSkOnly
                || List.contains s.country [Some "CZ"; Some "SK"; None])
            |> Array.map JdfFixups.moveRegionFromName
            |> Array.map JdfFixups.normaliseStopName
            |> Array.map (fun s ->
                sprintf "%s,%s,%s"
                        s.town
                        (s.district |> Option.defaultValue "")
                        (s.nearbyPlace |> Option.defaultValue ""))
            |> Set.ofArray
        with
        | e ->
            Log.Error(e, "Error reading stops from {JDF}", batchPath)
            set [])
    |> Set.unionMany

let czpttStopList czpttDir =
    CzPtt.parseAll czpttDir
    |> Seq.choose (fun (_, doc) ->
        match doc with
        | CzPtt.Timetable cisMsg ->
            cisMsg.CzpttInformation.CzpttLocation
            |> Array.map (fun loc ->
                loc.Location.LocationPrimaryCode
                |> Utils.nullOpt
                |> Option.map Utils.normaliseSr70
                |> Option.defaultValue "",
                loc.Location.PrimaryLocationName
                |> Utils.nullOpt
                |> Option.defaultValue ""
            )
            |> set
            |> Some
        | _ -> None)
    |> Set.unionMany
    |> Set.filter (fun t -> t <> ("", ""))
