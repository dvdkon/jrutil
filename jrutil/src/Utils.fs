// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUtil.Utils

open System
open System.IO
open System.Globalization
open System.Collections.Concurrent
open Docopt

#nowarn "0342"

let memoize f =
    let cache = new ConcurrentDictionary<_, _>()
    fun x ->
        let cached, result = cache.TryGetValue(x)
        if cached then result
        else
            let result = f x
            cache.[x] <- result
            result

let chainCompare next prev =
    if prev <> 0 then prev else next

let fileLinesSeq filename = seq {
    use file = File.OpenText filename
    while not file.EndOfStream do yield file.ReadLine()
}

[<CustomComparison>]
[<StructuralEquality>]
[<StructuredFormatDisplay("{year}-{month}-{day}")>]
type Date = {
    year: int
    month: int
    day: int
}
with
    interface IComparable with
        member this.CompareTo obj2 =
            match obj2 with
            | :? Date as date2 ->
                this.year.CompareTo date2.year
                |> chainCompare (this.month.CompareTo date2.month)
                |> chainCompare (this.day.CompareTo date2.day)
            | _ -> failwithf "Tried to compare Date with %A" obj2

let dateTimeToDate (dt: DateTime) = {
    year = dt.Year
    month = dt.Month
    day = dt.Day
}

// Used DateTime to parse and the converts the result to Date
let parseDate format str =
    DateTime.ParseExact(str, format, CultureInfo.InvariantCulture)
    |> dateTimeToDate

[<CustomComparison>]
[<StructuralEquality>]
[<StructuredFormatDisplay("{hour}-{minute}-{seconf}")>]
type Time = {
    hour: int
    minute: int
    second: int
}
with
    interface IComparable with
        member this.CompareTo obj2 =
            match obj2 with
            | :? Time as time2 ->
                this.hour.CompareTo time2.hour
                |> chainCompare (this.minute.CompareTo time2.minute)
                |> chainCompare (this.second.CompareTo time2.second)
            | _ -> failwithf "Tried to compare Time with %A" obj2

let dateTimeToTime (dt: DateTime) = {
    hour = dt.Hour
    minute = dt.Minute
    second = dt.Second
}

let parseTime format str =
    DateTime.ParseExact(str, format, CultureInfo.InvariantCulture)
    |> dateTimeToTime

let isLeapYear year = (year % 4 = 0) && ((year % 400 = 0) || (year % 100 <> 0))
let daysInMonths year =
    let febDays = if isLeapYear year then 29 else 28
    [|31; febDays; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31|]
let daysInMonth year month = (daysInMonths year).[month - 1]
let dateAddDay date =
    let dayOverflow = date.day + 1 > (daysInMonth date.year date.month)
    let monthOverflow = dayOverflow && date.month + 1 > 12
    { date with
        day = if dayOverflow then 1 else date.day + 1
        month =
            if dayOverflow
            then if monthOverflow
                 then 1
                 else date.month + 1
            else date.month
        year = if monthOverflow then date.year + 1 else date.year
    }

let rec dateRange (startDate: Date) (endDate: Date) =
    // Create a list of Dates containing all days between startDate
    // and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateRange (dateAddDay startDate) endDate)
    else []


let rec dateTimeRange (startDate: DateTime) (endDate: DateTime)  =
    // Create a list of DateTime objects containing all days between
    // startDate and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateTimeRange (startDate.AddDays(1.0)) endDate)
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
