// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík
module JrUtil.Tests.Asserts

open Microsoft.VisualStudio.TestTools.UnitTesting

let assertEqualWithMsg msg (expected: 'a) (actual: 'a) =
    let expectedIsNull = obj.ReferenceEquals(expected, null)
    let actualIsNull = obj.ReferenceEquals(actual, null)
    if expectedIsNull || actualIsNull then
        if expectedIsNull <> actualIsNull then
            raise <| new AssertFailedException(
                sprintf "%s!\nExpected: %A\nActual: %A"
                        msg (expected) (actual))
    else
        if expected.GetType() <> actual.GetType() then
            raise <| new AssertFailedException(
                sprintf "Objects have different types!\nExpected: %A\nActual: %A"
                        (expected.GetType()) (actual.GetType()))

        if expected <> actual then
            raise <| new AssertFailedException(
                sprintf "%s\nExpected: %A\nActual: %A"
                        msg expected actual)

let assertEqual (expected: 'a) (actual: 'a) =
    assertEqualWithMsg "Objects are not equal!" expected actual

let assertSeqEqual (expected: 'a seq) (actual: 'a seq) =
    if Seq.length actual <> Seq.length expected then
        raise <| new AssertFailedException(
            "Collections have different length!")

    Seq.zip expected actual
    |> Seq.iteri (fun i (e, a) ->
        assertEqualWithMsg
            (sprintf "Element %d of collection is not equal!" i) e a)
