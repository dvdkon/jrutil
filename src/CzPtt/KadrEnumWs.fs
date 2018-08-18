// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.KadrEnumWs

open FSharp.Data

type CompanyListXml =
    XmlProvider<"samples/kadrenum/SeznamSpolecnostiResult.xml">

let serviceUrl = "http://provoz.szdc.cz/kadrws/ciselniky.asmx"

let makeSoapRequest endpoint body =
    Http.RequestString(
        serviceUrl,
        httpMethod="POST",
        headers=[
            "Content-Type", "text/xml; charset=utf-8";
            "SOAPAction", "http://provoz.szdc.cz/kadr/" + endpoint],
        body=TextRequest body
    )

let mutable companyListCache: CompanyListXml.Envelope option = None

let requestCompanyList () =
    match companyListCache with
    | Some cl -> cl
    | None ->
        let body =
            """<?xml version="1.0" encoding="utf-8"?>
            <soap:Envelope
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                    xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                    xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                <soap:Body>
                    <SeznamSpolecnosti xmlns="http://provoz.szdc.cz/kadr">
                        <jenAktualnePlatne>true</jenAktualnePlatne>
                    </SeznamSpolecnosti>
                </soap:Body>
            </soap:Envelope>
            """
        let strResp = makeSoapRequest "SeznamSpolecnosti" body
        let cl = CompanyListXml.Parse(strResp)
        companyListCache <- Some cl
        cl
