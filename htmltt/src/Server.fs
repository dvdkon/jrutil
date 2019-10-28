// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module HtmlTt.Server

#nowarn "58"

open Giraffe
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open System.Net

open HtmlTt.Pages

let runServer dbConn (bindAddr: string) =
    let routes = choose [
        route "/agencies" >=> htmlView (agenciesPage dbConn)
        routef "/agency/%s" (fun agencyId ->
            htmlView (agencyPage dbConn agencyId))
        routef "/route/%s" (fun routeId -> htmlView (routePage dbConn routeId))
        route "/stops" >=> htmlView (stopsPage dbConn)
        routef "/stop/%s" (fun stopId -> htmlView (stopPage dbConn stopId))
    ]

    WebHostBuilder()
        .ConfigureKestrel(fun kestrel ->
            let success, endpoint = IPEndPoint.TryParse(bindAddr)
            if not success
            then failwithf "Invalid bind address: %s" bindAddr
            kestrel.Listen(endpoint)
        ).UseKestrel()
        .Configure(fun app ->
            (*app.UseGiraffeErrorHandler(fun ex logger ->

               ).UseGiraffe routes*)
            app.UseGiraffe routes
        ).ConfigureServices(fun services ->
            services.AddGiraffe() |> ignore
        ).ConfigureLogging(fun logging ->
            logging.AddConsole() |> ignore
        ).Build()
        .Run()
