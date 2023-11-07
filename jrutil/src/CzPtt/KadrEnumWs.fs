// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.KadrEnumWs

open FSharp.Data

open JrUtil.Utils
open KadrWsCiselniky

let client = memoizeVoidFunc (fun () ->
        new CiselnikySoapClient(
            CiselnikySoapClient.EndpointConfiguration.CiselnikySoap))

let requestCompanyList = memoizeVoidFunc (fun () ->
    // TODO: Allow for processing older data by processing currently invalid
    // entries (which we now filter)
    (client ()).SeznamSpolecnosti(true)
)

let companyForEvCisloEu evCisloEu =
    let companiesResp = requestCompanyList()
    let companies = companiesResp.Spolecnost
    let matching =
        companies
        |> Seq.filter (fun c -> c.EvCisloEU = evCisloEu)
        // Try to get the most applicable company first
        |> Seq.sortByDescending (fun c ->
            c.Licence
            |> nullOpt
            |> Option.map (fun lic ->
                [lic.VerejnaDopr; lic.PrapravaOsob; lic.DrahaCelostatni]
                |> Seq.filter id
                |> Seq.length)
            |> Option.defaultValue 0
        )
    let best = Seq.tryHead matching
    best

let trafficTypes = memoizeVoidFunc (fun () ->
    (client ()).SeznamDruhuVlaku(true).DruhVlaku
    |> Seq.map (fun e -> e.KodTAF, e.Zkratka)
    |> Map)

let commercialTrafficTypes = memoizeVoidFunc (fun () ->
    (client ()).SeznamKomercniDruhVlaku().KomercniDruhVlaku
    |> Seq.map (fun e -> e.KodTAF, e.Kod)
    |> Map)
