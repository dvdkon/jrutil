// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

module GeoReport.StopListGen

open System.IO
open Serilog

open JrUtil

let jdfStopNames czSkOnly jdfDir =
    let stopsParser = Jdf.fileParser "Zastavky.txt"

    Jdf.findJdfBatches jdfDir
    |> Seq.toArray
    |> Array.map (fun batchPath ->
        Log.Information("Reading stops from {JDF}", batchPath)
        try
            let stops: JdfModel.Stop array = stopsParser batchPath
            stops
            |> Array.filter (fun s ->
                not czSkOnly || List.contains s.country ["CZ"; "SK"; ""])
            |> Array.map JdfFixups.moveNearbyTownFromName
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
    |> Seq.map (fun doc ->
        doc.CzpttcisMessage |> Option.map (fun cisMsg ->
            cisMsg.CzpttInformation.CzpttLocations
            |> Array.map (fun loc ->
                loc.Location.LocationPrimaryCode
                |> Option.map Utils.normaliseSr70
                |> Option.defaultValue "",
                loc.Location.PrimaryLocationName
                |> Option.defaultValue ""
            )
            |> set
        )
        |> Option.defaultValue Set.empty)
    |> Set.unionMany
    |> Set.filter (fun t -> t <> ("", ""))
