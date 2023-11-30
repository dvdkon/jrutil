// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CsvMetadata

open System

// Unfortunately needed due to reflection methods returning null
[<AllowNullLiteral>]
type CsvSpreadAttribute(len: int) =
    inherit Attribute()
    member this.Len = len
