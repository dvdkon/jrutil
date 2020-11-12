// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Rewrite
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open WebSharper.AspNetCore

open JrUtil.Utils
open RtView

let docstring = (fun (s: string) -> s.Trim()) """
RtView a web application for exploring data collected by RtCollect

Usage:
    rtview [options] <db-connstr>

Options:
    --logfile=FILE           Path to logfile
"""

type Startup() =
    member this.ConfigureServices(services: IServiceCollection) = ()
    member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
        if env.IsDevelopment() then app.UseDeveloperExceptionPage() |> ignore

        app.UseRewriter(
                RewriteOptions()
                // Let the client-side handle routing of everything except for
                // static resources (exclude POST, because that causes loops)
                 .Add(fun ctx ->
                    if (not <| ctx.HttpContext.Request.Path.StartsWithSegments(
                                PathString("/Content")))
                       && ctx.HttpContext.Request.Method = "GET" then
                        ctx.HttpContext.Request.Path <- PathString("/")
                        ctx.Result <- RuleResult.SkipRemainingRules
                    else
                        ctx.Result <- RuleResult.ContinueRules
                 ))
            .UseDefaultFiles()
            .UseStaticFiles()
            .UseWebSharper(fun builder -> builder.UseSitelets(false) |> ignore)
            .Run(fun context ->
                context.Response.StatusCode <- 404
                context.Response.WriteAsync(
                    "Page not found: " + context.Request.Path))

[<EntryPoint>]
let main argv =
    withProcessedArgs docstring argv (fun args ->
        ServerGlobals.init (argValue args "<db-connstr>")

        WebHost
            .CreateDefaultBuilder()
            .UseStartup<Startup>()
            .Build()
            .Run()
        0)
