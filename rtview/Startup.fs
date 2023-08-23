// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

open System
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection

open RtView

[<EntryPoint>]
let main argv =
    let builder = WebApplication.CreateBuilder(argv)
    builder.Services.AddControllers() |> ignore
    let app = builder.Build()
    app.MapControllers() |> ignore
    app.UseStaticFiles() |> ignore
    ServerGlobals.init <| app.Configuration.GetValue<string>("db-connstr")
    app.Run()

    0
