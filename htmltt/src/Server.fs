// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module HtmlTt.Server

#nowarn "58"

open Giraffe
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpOverrides.Internal
open Microsoft.Extensions.Logging

open HtmlTt.Pages

let runServer dbConn bindAddr =
    let routes = choose [
        route "/agencies" >=> htmlView (agenciesPage dbConn)
        routef "/agency/%s" (fun agencyId ->
            htmlView (agencyPage dbConn agencyId))
        routef "/route/%s" (fun routeId -> htmlView (routePage dbConn routeId))
    ]

    WebHostBuilder()
        .ConfigureKestrel(fun kestrel ->
            let success, endpoint = IPEndPointParser.TryParse(bindAddr)
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
