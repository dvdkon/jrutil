// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.CzPtt

open System.IO
open FSharp.Data

open JrUtil.Utils

type CzPttXml = XmlProvider<Schema="src/CzPtt/czptt.xsd">

type LocationType =
    | [<StrValue("01")>] Origin
    | [<StrValue("02")>] Intermediate
    | [<StrValue("03")>] Destination
    | [<StrValue("04")>] Handover
    | [<StrValue("05")>] Interchange
    | [<StrValue("06")>] HandoverAndInterchange
    | [<StrValue("07")>] StateBorder

let parseFile (path: string) =
    // .Load() uses the executable as the PWD, hence the workaround
    CzPttXml.Parse(File.ReadAllText(path))
