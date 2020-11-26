// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

namespace RtView

open WebSharper

[<JavaScript>]
module ClientMain =
    open WebSharper.UI
    open WebSharper.UI.Client
    open WebSharper.Sitelets

    open RtView.ClientGlobals

    [<SPAEntryPoint>]
    let clientMain () =
        router <- Router.Infer<Locations> ()
        location <- Router.Install Homepage router
        location.View.Doc (function
            | Homepage -> RtView.Routes.Client.tripList None None None ()
            | Routes (f, t, s) -> RtView.Routes.Client.tripList f t s ()
            | Trip (tid, d) -> RtView.Trip.Client.tripPage tid d ()
        )
        |> Doc.RunReplaceById "main"