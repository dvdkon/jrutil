// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík
namespace JrUtil.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil.SearchParser
open JrUtil.SqlSearch
open JrUtil.Tests.Asserts

// For fields recursion
#nowarn "40"

[<TestClass>]
type SqlSearchTests() =
    let rec fields = Map [
        "", stringField "content"
        "not", notField (fun () -> fields)
        "num", intField "num"
        "date", dateField "date"
    ]

    let assertSql search sql =
        parseSearch search
        |> searchToSql fields
        |> assertOk
        |> fst
        |> assertEqual sql

    [<TestMethod>]
    member this.``Basic search``() =
        assertSql "" "TRUE"
        assertSql "abc" "(content ILIKE @p1)"
        assertSql "abc def" "(content ILIKE @p1) AND (content ILIKE @p2)"

    [<TestMethod>]
    member this.``Field search``() =
        assertSql "num:50" "(num = @p1)"
        assertSql "num:eq:50" "(num = @p1)"
        assertSql "num:lt:50" "(num < @p1)"
        assertSql "num:gt:50" "(num > @p1)"
        assertSql "not:num:eq:50" "(NOT (num = @p1))"
        assertSql "not:testing" "(NOT (content ILIKE @p1))"
        assertSql "not:num:50" "(NOT (num = @p1))"
        assertSql "date:2020-01-01" "(date = @p1)"
