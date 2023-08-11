// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

/// This module provides utility functions that can help repair bad JDF data,
/// intentionally bad or not.
module JrUtil.JdfFixups

open System.Text.RegularExpressions

open JrUtil.JdfModel

// Some JDF batches in CIS JŘ public exports have the nearby town two-letter id
// appended to the town name like so: "Praha [AB]". This function will move it
// to its rightful place three columns to the right.
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
