// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.Utils

open System
open System.Collections.Generic
open Docopt

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

let argFlagSet (arg: Arguments.Result) =
    match arg with
    | Arguments.Result.Flag | Arguments.Result.Flags _ -> true
    | Arguments.Result.None -> false
    | _ -> failwithf "Expected %A to be a Flag or Flags" arg

let argValue (arg: Arguments.Result) =
    match arg with
    | Arguments.Result.Argument x -> x
    | _ -> failwithf "Expected %A to be an Argument" arg

let argValues (arg: Arguments.Result) =
    match arg with
    | Arguments.Result.Arguments x -> x
    | _ -> failwithf "Expected %A to be Arguments" arg
