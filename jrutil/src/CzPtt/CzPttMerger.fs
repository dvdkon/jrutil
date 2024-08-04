// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CzPttMerge

open System.Collections.Generic

open JrUtil.CzPtt
open Serilog
open Serilog.Context

type CzPttMerger() =
    let getPaidStr (ptis: CzPttXml.TransportIdentifier seq) =
        ptis |> Seq.find (fun pti ->
            pti.ObjectType = CzPttXml.ObjectType.Pa)
        |> identifierStr

    // Indexed by PAID
    member val Messages = Dictionary<string, CzPttXml.CzpttcisMessage>()

    member this.Add(msg: CzPttXml.CzpttcisMessage) =
        let paid = getPaidStr msg.Identifiers
        if this.Messages.ContainsKey(paid) then
            Log.Error("Tried to add duplicate message: {PAID}", paid)
        else
            this.Messages[paid] <- msg

    member this.Cancel(cancelMsg: CzPttXml.CzCanceledPttMessage) =
        let paid = getPaidStr cancelMsg.PlannedTransportIdentifiers
        if not <| this.Messages.ContainsKey(paid) then
            Log.Error("Tried to cancel non-existing message: {PAID}", paid)
        else
            let msg = this.Messages[paid]
            let msgBitmap = parseCalendar msg.CzpttInformation.PlannedCalendar
            let cancelBitmap = parseCalendar cancelMsg.PlannedCalendar
            // false in cancelBitmap means "no change", so we extend it padding
            // by false, invert it and AND with original
            let newBitmap =
                msgBitmap.And(
                    cancelBitmap.ExtendTo(msgBitmap.Interval, false).Not())
            if newBitmap.HasAnySet() |> not then
                // Train was fully cancelled, remove it entirely
                this.Messages.Remove(paid) |> ignore
            else
                msg.CzpttInformation.PlannedCalendar <-
                    serializeCalendar(newBitmap)
                ()

    member this.Process(msg: CzpttMessage) =
        match msg with
        | Timetable m -> this.Add(m)
        | Cancellation c -> this.Cancel(c)

    member this.ProcessAll(msgs: (string * CzpttMessage) seq) =
        // We need to process the timetables first, before we can cancel them
        let msgs =
            msgs
            |> Seq.sortBy (fun (_, m) ->
                match m with Timetable _ -> 0 | Cancellation _ -> 1)
        for name, msg in msgs do
            use _logCtx = LogContext.PushProperty("CzPttFile", name)
            Log.Information("Merging CZPTT file {CzPttFile}", name)
            try
                this.Process(msg)
            with
            | e -> Log.Error(e, "Error while merging {CzPttFile}", name)
