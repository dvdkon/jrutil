// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík
namespace JrUtil.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil.SearchParser
open JrUtil.Tests.Asserts

[<TestClass>]
type SearchParserTests() =
    [<TestMethod>]
    member this.``Basic tokenizing``() =
        nextToken "" |> assertEqual (None, "")
        nextToken " a" |> assertEqual (Some Whitespace, "a")
        nextToken "a-x" |> assertEqual (Some <| TextToken "a-x", "")
        nextToken ":fw" |> assertEqual (Some Colon, "fw")
        nextToken "abc def" |> assertEqual (Some <| TextToken "abc", " def")
        nextToken "\"abc def\""
        |> assertEqual (Some <| QuotedText "abc def", "")
        nextToken "\"abc def" |> assertEqual (Some <| TextToken "abc", " def")

    [<TestMethod>]
    member this.``Parsing individual clauses``() =
        parseSearch "" |> assertEqual (Composite [])
        parseSearch "abc"
        |> assertEqual (Composite [Text "abc"])
        parseSearch "f:abc"
        |> assertEqual (Composite [Application [Text "f"; Text "abc"]])
        parseSearch "f:abc:def"
        |> assertEqual (Composite [
            Application [Text "f"; Text "abc"; Text "def"]])

    [<TestMethod>]
    member this.``Parsing multiple clauses``() =
        parseSearch "abc de fg"
        |> assertEqual (Composite [
            Text "abc"
            Text "de"
            Text "fg"])
        parseSearch "abc (de fg) hi"
        |> assertEqual (Composite [
            Text "abc"
            Composite [
                Text "de"
                Text "fg"
            ]
            Text "hi"
        ])
