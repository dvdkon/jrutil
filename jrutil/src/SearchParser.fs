// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.SearchParser

type Token =
    | Whitespace
    | Colon
    | TextToken of string
    | QuotedText of string
    | OpenBracket
    | CloseBracket

type Expression =
    | Text of string
    | Application of Expression list
    | Composite of Expression list

let rec nextToken str =
    if str = "" then None, ""
    elif str.[0] = ' ' then Some Whitespace, str.[1..]
    elif str.[0] = ':' then Some Colon, str.[1..]
    elif str.[0] = '(' then Some OpenBracket, str.[1..]
    elif str.[0] = ')' then Some CloseBracket, str.[1..]
    elif str.[0] = '"' then
        let endIdx = str.[1..].IndexOf('"') + 1
        // Be gentle, users wouldn't expect fatal errors in the parsing of
        // a search field (even though they maybe should)
        if endIdx = 0 then nextToken str.[1..]
        else Some <| QuotedText str.[1..endIdx - 1], str.[endIdx + 1..]
    else
        let endIdx = str.IndexOfAny([| ' '; ':'; '"'; '('; ')' |])
        if endIdx = -1 then Some <| TextToken str, ""
        else Some <| TextToken str.[..endIdx - 1], str.[endIdx..]

let parseSearch fullStr =
    let rec parseClause str =
        let tok, rest1 = nextToken str
        match tok with
        | None -> None, rest1
        | Some Whitespace -> None, rest1
        | Some Colon ->
            Some <| Text ":", rest1
        | Some CloseBracket ->
            Some <| Text ")", rest1
        | Some OpenBracket ->
            let rec parseInner str acc =
                let clause, rest2 = parseClause str
                let acc = clause :: acc
                let tok2, rest3 = nextToken rest2
                if tok2 = Some CloseBracket || tok2 = None
                then Composite (acc |> List.rev |> List.choose id), rest3
                else parseInner rest2 acc
            let cl, rest2 = parseInner rest1 []
            Some cl, rest2
        | Some (TextToken s) | Some (QuotedText s) ->
            let maybeColon, rest2 = nextToken rest1
            if maybeColon = Some Colon then
                let maybeInner, rest3 = parseClause rest2
                match maybeInner with
                | Some (Application inner) ->
                    Some <| Application (Text s :: inner), rest3
                | Some inner ->
                    Some <| Application [Text s; inner], rest3
                | None -> Some <| (Text s), rest1
            else Some (Text s), rest1

    let rec parseToplevel str =
        match parseClause str with
        | None, "" -> []
        | None, rest1 -> parseToplevel rest1
        | Some clause1, rest1 -> clause1 :: parseToplevel rest1

    Composite <| parseToplevel fullStr
