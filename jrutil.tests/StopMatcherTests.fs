// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík
namespace JrUtil.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil.GeoData.StopMatcher
open JrUtil.Tests.Asserts

[<TestClass>]
type StopMatcherTests() =
    [<TestMethod>]
    member this.``Basic stop search``() =
        let stopsToMatch = [|
            {name = "Hostivice"; data = 1}
            {name = "Hostivice-Sadová"; data = 2}
            {name = "Praha-Dejvice"; data = 3}
        |]
        use matcher = new StopMatcher<int>(stopsToMatch)

        (matcher.matchStop("Hostivice") |> Seq.head).stop.data
            |> assertEqual 1
        (matcher.matchStop("praha dejvice") |> Seq.head).stop.data
            |> assertEqual 3
