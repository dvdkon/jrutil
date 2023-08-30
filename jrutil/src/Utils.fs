// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.Utils

open System
open System.IO
open System.Diagnostics
open System.Globalization
open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Collections.Generic
open DocoptNet
open Serilog
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes
open Serilog.Formatting.Compact
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
let tryParseDate (format: string) (str: string) =
    let success, dt = DateTime.TryParseExact(
        str, format, CultureInfo.InvariantCulture, DateTimeStyles.AssumeLocal)
    if success then Some <| LocalDate.FromDateTime(dt)
    else None
let parseDate format str =
    tryParseDate format str |> Option.get

let tryParseTime format str =
    let pattern = LocalTimePattern.Create(format, CultureInfo.InvariantCulture)
    let res = pattern.Parse(str)
    if res.Success then Some res.Value else None
let parseTime format str =
    match tryParseTime format str with
    | Some t -> t
    | None -> failwithf "Failed to parse time \"%s\" with pattern \"%s\""
                        str format

let tryParsePeriod (format: string) (str: string) =
    let success, timespan =
        TimeSpan.TryParseExact(str, format, CultureInfo.InvariantCulture)
    if success then
        Some <| Period.FromMilliseconds(int64 timespan.TotalMilliseconds)
    else None
let parsePeriod format str =
    tryParsePeriod format str |> Option.get

let dateToIso (date: LocalDate) =
    date.ToString("uuuu-MM-dd", CultureInfo.InvariantCulture)

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

let argFlagSet (args: IDictionary<string, ArgValue>) name =
    let arg = args.[name]
    let s, v = arg.TryAsBoolean()
    assert s
    v

let argValue (args: IDictionary<string, ArgValue>) name =
    let arg = args.[name]
    let s, v = arg.TryAsString()
    assert s
    v

let optArgValue (args: IDictionary<string, ArgValue>) name =
    match args.TryGetValue(name) with
    | true, a ->
        let s, v = a.TryAsString()
        if s then Some v
        else None
    | false, _ -> None

let argValues (args: IDictionary<string, ArgValue>) name =
    let arg = args.[name]
    let s, v = arg.TryAsStringList()
    assert s
    v

let withProcessedArgs docstring (args: string array) fn =
    match Docopt.CreateParser(docstring)
                .Parse(args) with
    | :? IArgumentsResult<_> as r -> fn r.Arguments
    | :? IHelpResult | :? IVersionResult ->
        printfn "%s" docstring
        0
    | :? IInputErrorResult as e ->
        printfn "%s" e.Error
        1
    | _ -> assert false; 1

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
            loggerFactory <- loggerFactory.WriteTo.File(
                CompactJsonFormatter(), lf.[5..])
        else if lf.StartsWith("rjson:") then
            loggerFactory <- loggerFactory.WriteTo.File(
                RenderedCompactJsonFormatter(), lf.[6..])
        else
            loggerFactory <- loggerFactory.WriteTo.File(lf))
    Log.Logger <- loggerFactory.CreateLogger()

/// Create Serilog event without logging it immediately
let logEvent level (msg: string) (props: 'a array) =
    let valid, parsedMsg, boundProps =
        Log.BindMessageTemplate(msg, props |> Array.map box)
    if not valid then
        failwithf "Invalid log event: '%s' %A" msg props
    LogEvent(DateTimeOffset.Now, level, null, parsedMsg, boundProps)

/// Useful for wrapping long computations for logging, e.g.
/// `logWrappedOp "Computing Pi" (getDigitsOfPi 1000)`
let logWrappedOp (msg: string) f =
    Log.Information("{Operation}...", msg)
    let v = f ()
    Log.Information("{Operation} finished", msg)
    v

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

/// Like pairwise, but the previous element is returned as an option, so the
/// first element is (None, head)
let tryPairwise s =
    Seq.concat [
        seq { None, Seq.head s }
        Seq.pairwise s |> Seq.map (fun (a, b) -> Some a, b)
    ]

let leftJoinOn xkey ykey xs ys =
    let ysMap = ys |> Seq.map (fun y -> ykey y, y) |> Map
    let st = new System.Diagnostics.StackTrace();
    xs |> Seq.map (fun x -> x, ysMap |> Map.tryFind (xkey x))

exception JoinException of string
    with override this.Message = this.Data0

let innerJoinOn xkey ykey xs ys =
    leftJoinOn xkey ykey xs ys
    |> Seq.map (fun (x, yo) ->
        x, match yo with
           | Some y -> y
           | None -> raise (JoinException (sprintf
                "Could not match left key %A" (xkey x))))

let optResult error = function
    | Some v -> Ok v
    | None -> Error error

let splitSeq pred xs =
    let split = xs |> Seq.groupBy pred |> Seq.toList
    split
    |> List.tryFind (fun (b, _) -> b)
    |> Option.defaultValue (true, [])
    |> snd,
    split
    |> List.tryFind (fun (b, _) -> not b)
    |> Option.defaultValue (false, [])
    |> snd