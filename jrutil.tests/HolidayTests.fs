// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík
namespace JrUtil.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime
open JrUtil.Holidays
open JrUtil.Tests.Asserts

[<TestClass>]
type HolidayTests() =
    [<TestMethod>]
    member this.``Easter Sunday for 2020``() =
        easterSundayDate 2020 |> assertEqual (12, 4)

    [<TestMethod>]
    member this.``Czech holidays for 2020``() =
        czechHolidays 2020 |> Seq.sort |> assertSeqEqual (Seq.sort [
            1, 1
            10, 4
            13, 4
            1, 5
            8, 5
            5, 7
            6, 7
            28, 9
            28, 10
            17, 11
            24, 12
            25, 12
            26, 12
        ])

