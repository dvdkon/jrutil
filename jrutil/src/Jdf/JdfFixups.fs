// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

/// This module provides utility functions that can help repair bad JDF data,
/// intentionally bad or not.
module JrUtil.JdfFixups

open System.Text.RegularExpressions

open JrUtil.JdfModel

/// Some JDF batches in CIS JŘ public exports have the nearby town two-letter id
/// appended to the town name like so: "Praha [AB]". This function will move it
/// to its rightful place three columns to the right.
let moveNearbyTownFromName =
    let ntRegex = Regex(@"(.*) \[(..)\]$")
    fun (stop: Stop) ->
        let m = ntRegex.Match(stop.town)
        if m.Success
        then {
            stop with
                town = m.Groups.[1].Value
                nearbyTownId = Some m.Groups.[2].Value
        }
        else stop

/// In the public CIS JŘ exports, stops don't have consistent naming.
/// Sometimes, the whole name, delimited by commas, is in "town".
/// When "districy" should be empty, it sometimes has the value of
/// "nearbyPlace".
/// This function will take a Stop and give it a normalised name that can
/// later be compared between batches, not necessarily so some yet-unpublished
/// official stop registry name.
let normaliseStopName =
    let doubleCommaName = Regex(@"([^,]+), *([^,]*), *([^,]*)")
    let emptyToNone s = if s = "" then None else Some s
    fun (stop: Stop) ->
        let newTown, newDistrict, newNearbyPlace =
            let dcMatch = doubleCommaName.Match(stop.town)
            if stop.district.IsNone && stop.nearbyTownId.IsNone
               && dcMatch.Success then
                dcMatch.Groups.[1].Value,
                emptyToNone dcMatch.Groups.[2].Value,
                emptyToNone dcMatch.Groups.[3].Value
            else if stop.district.IsNone then
                stop.town, stop.nearbyPlace, None
            else
                stop.town, stop.district, stop.nearbyPlace
        { stop with
            town = newTown
            district = newDistrict
            nearbyPlace = newNearbyPlace }
