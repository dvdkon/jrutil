// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Utils

open System
open System.Collections.Generic

let memoize f =
    let cache = new Dictionary<_, _>()
    fun x ->
        let cached, result = cache.TryGetValue(x)
        if cached then result
        else
            let result = f x
            cache.[x] <- result
            result

let rec dateRange (startDate: DateTime) (endDate: DateTime)  =
    // Create a list of DateTime objects containing all days between
    // startDate and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateRange (startDate.AddDays(1.0)) endDate)
    else []

let constant x _ = x
