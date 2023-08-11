// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.Utils

open System
open System.IO
open System.Diagnostics
open System.Globalization
open System.Collections.Concurrent
open System.Runtime.InteropServices
open Docopt
open Serilog
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes
open Serilog.Formatting.Json
open NodaTime
open NodaTime.Text

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

let memoizeVoidFunc f =
    let mutable cache = None
    fun () ->
        cache
        |> Option.defaultWith (fun () ->
            let result = f()
            cache <- Some result
            result)

let chainCompare next prev =
    if prev <> 0 then prev else next

let fileLinesSeq filename = seq {
    use file = File.OpenText filename
    while not file.EndOfStream do yield file.ReadLine()
}

/// A custom parallel map that lets the user specify the number of processing
/// threads used
/// Each thread gets its next input from a queue, processes that input and then
/// puts the output into the output queue
let parmap<'a, 'b> threadCount func (inputs: 'a seq) = (seq {
    let enumerator = inputs.GetEnumerator()
    let next() =
        lock enumerator (fun () ->
            if enumerator.MoveNext() then Some enumerator.Current else None)
    let outputQueue = new BlockingCollection<'b>()
    let processingTask =
        [for _ in 1..threadCount ->
            async {
                while (match next() with
                       | Some i ->
                           outputQueue.Add(func i)
                           true
                       | None -> false) do ()
            }]
        |> Async.Parallel
        |> Async.StartAsTask
    async {
        processingTask.Wait()
        outputQueue.CompleteAdding()
    } |> Async.Start
    while not outputQueue.IsCompleted do
        // seq expressions can't contain try with directly, but can
        // as a subexpression...
        yield (try Some <| outputQueue.Take()
               with :? InvalidOperationException -> None)
} |> Seq.choose id)

// A version of parmap for side-effecting functions
let pariter<'a> threadCount func (inputs: 'a seq) =
    let enumerator = inputs.GetEnumerator()
    let next() =
        lock enumerator (fun () ->
            if enumerator.MoveNext() then Some enumerator.Current else None)
    let processingTask =
        [for _ in 1..threadCount ->
            async {
                while (match next() with
                       | Some i ->
                           func i
                           true
                       | None -> false) do ()
            }]
        |> Async.Parallel
        |> Async.StartAsTask
    processingTask.Wait()

// Used DateTime to parse and the converts the result to LocalDate
let parseDate format str =
    let dt = DateTime.ParseExact(str, format, CultureInfo.InvariantCulture)
    LocalDate.FromDateTime(dt)

let parseTime format str =
    let pattern = LocalTimePattern.Create(format, CultureInfo.InvariantCulture)
    let res = pattern.Parse(str)
    if res.Success then res.Value
    else failwithf "Failed to parse time \"%s\" with pattern \"%s\"" str format

let parsePeriod (format: string) (str: string) =
    let timespan =
        TimeSpan.ParseExact(str, format, CultureInfo.InvariantCulture)
    Period.FromMilliseconds(int64 timespan.TotalMilliseconds)

let rec dateRange (startDate: LocalDate) (endDate: LocalDate) =
    // Create a list of Dates containing all days between startDate
    // and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateRange (startDate.PlusDays(1)) endDate)
    else []

let rec dateTimeRange (startDate: DateTime) (endDate: DateTime)  =
    // Create a list of DateTime objects containing all days between
    // startDate and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateTimeRange (startDate.AddDays(1.0)) endDate)
    else []

let dateToday () =
    LocalDate.FromDateTime(DateTime.Today)

let constant x _ = x

let argFlagSet (args: Arguments.Dictionary) name =
    match args.[name] with
    | Arguments.Result.Flag | Arguments.Result.Flags _ -> true
    | Arguments.Result.None -> false
    | _ -> raise (ArgvException (sprintf "Expected %A to be a Flag or Flags while processing %s"
                                         args.[name] name))

let argValue (args: Arguments.Dictionary) name =
    match args.[name] with
    | Arguments.Result.Argument x -> x
    | Arguments.Result.None ->
        raise (ArgvException (sprintf "Argument not found: %s" name))
    | _ -> raise (ArgvException (sprintf "Expected %A to be an Argument"
                                         args.[name]))

let optArgValue (args: Arguments.Dictionary) name =
    match args.[name] with
    | Arguments.Result.None -> None
    | Arguments.Result.Argument x -> Some x
    | _ -> raise (ArgvException (sprintf "Expected %A to be an Argument or None"
                                         args.[name]))

let argValues (args: Arguments.Dictionary) name =
    match args.[name] with
    | Arguments.Result.Arguments x -> x
    | Arguments.Result.None ->
        raise (ArgvException (sprintf "Argument not found: %s" name))
    | _ -> raise (ArgvException (sprintf "Expected %A to be Arguments"
                                         args.[name]))

let withProcessedArgs docstring (args: string array) fn =
    if args.Length = 0 then
        printfn "%s" docstring
        0
    else
        try
            let docopt = Docopt(docstring)
            let args = docopt.Parse(args)

            fn args
        with
        | ArgvException(msg) ->
            printfn "%s" msg
            1

let measureTime msg func =
    let sw = Stopwatch.StartNew()
    let res = func()
    sw.Stop()
    Log.Information("{Section} took {Time}", msg, sw.Elapsed)
    res

let findPathCaseInsensitive dirPath (filename: string) =
    let files =
        Directory.GetFiles(dirPath)
        |> Array.filter
            (fun f -> Path.GetFileName(f).ToLower() = filename.ToLower())
    match files.Length with
    | 1 -> Some files.[0]
    | 0 -> None
    | _ ->
        failwithf "Multiple files found when looking for %s in %s (case insensitive)"
                  filename dirPath

let setupLogging (logFile: string option) () =
    let mutable loggerFactory =
        LoggerConfiguration()
         .MinimumLevel.Debug()
         .Enrich.FromLogContext()
         .Destructure.FSharpTypes()
         .WriteTo.Console(
             standardErrorFromLevel = LogEventLevel.Verbose,
             applyThemeToRedirectedOutput = true,
             theme = if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
                     then SystemConsoleTheme.Literate :> ConsoleTheme
                     else AnsiConsoleTheme.Literate :> ConsoleTheme)
    logFile |> Option.iter (fun lf ->
        if lf.StartsWith("display:") then
            loggerFactory <- loggerFactory.WriteTo.File(lf.[8..])
        else if lf.StartsWith("json:") then
            loggerFactory <- loggerFactory.WriteTo.File(JsonFormatter(), lf.[5..])
        else
            loggerFactory <- loggerFactory.WriteTo.File(lf))
    Log.Logger <- loggerFactory.CreateLogger()

// Computed UIC checksum digit
// Algorithm source: https://github.com/proggy/uic/
// Works for computing sixth digit of SR70 ID
let uicChecksum (digits: int array) =
    (digits
    |> Array.mapi (fun i d -> if (digits.Length - i) % 2 = 1 then d * 2 else d)
    |> Array.sum) % 10

let normaliseSr70 (sr70: string) =
    // Strip checksum digit
    if sr70.Length > 5
    then sr70.[..4]
    else sr70.PadLeft(5, '0')
