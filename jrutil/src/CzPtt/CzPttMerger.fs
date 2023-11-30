// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CzPttMerge

open System.Collections.Generic

open JrUtil.CzPtt
open Serilog

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
            msg.CzpttInformation.PlannedCalendar <-
                serializeCalendar(newBitmap)
            ()

    member this.Process(msg: CzpttMessage) =
        match msg with
        | Timetable m -> this.Add(m)
        | Cancellation c -> this.Cancel(c)
