// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

/// This module allows matching a stop name to candidates from a list of
/// provided "stops to match". This is useful e.g. for adding locations to
/// stops.
module JrUtil.GeoData.StopMatcher

open System
open Lucene.Net.Util
open Lucene.Net.Store
open Lucene.Net.Documents
open Lucene.Net.Index
open Lucene.Net.Search
open Lucene.Net.Analysis
open Lucene.Net.Analysis.Core

open JrUtil

[<Struct>]
type StopToMatch<'d> = {
    name: string
    data: 'd
}

type StopMatch<'d> = {
    stop: StopToMatch<'d>
    score: float32
}

let preprocessStopName (name: string) =
    name
     // Make sure successive acronyms are tokenized as separate
     .Replace(".", ". ")
     // Strip punctuation
     .Replace('.', ' ').Replace(',', ' ').Replace('-', ' ')
     .ToLower()


let stopNameToTokens (name: string) =
    (preprocessStopName name)
     .Split(' ')
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.toArray

let stopNameToQuery (tokens: string array) =
    let query = BooleanQuery()
    for token in tokens do
        query.Add(BooleanClause(TermQuery(Term("name", token)), Occur.MUST))
    query

let nameSimilarity (queryTokens: string array) (matchedTokens: string array) =
    let qtSet = set queryTokens
    let mtSet = set matchedTokens
    // Result: How many of the matched stop's words are also in the query
    float32 (Set.intersect qtSet mtSet |> Set.count)
        / float32 matchedTokens.Length

type StopMatcher<'d>(stops: StopToMatch<'d> array) as this =
    let luceneVersion = LuceneVersion.LUCENE_48
    let directory = new RAMDirectory()
    do this.index()

    member this.index() =
        use analyzer = Analyzer.NewAnonymous(fun fieldName reader ->
            let tokenizer = new WhitespaceTokenizer(luceneVersion, reader)
            TokenStreamComponents(tokenizer)
        )
        let config = IndexWriterConfig(luceneVersion, analyzer)
        use writer = new IndexWriter(directory, config)

        for i, stop in stops |> Seq.indexed do
            let doc = Document()
            doc.Add(new Field("name", preprocessStopName stop.name,
                              TextField.TYPE_NOT_STORED))
            doc.Add(new Int32Field("index", i, Field.Store.YES))
            writer.AddDocument(doc)

    member this.matchStop(name, ?top) =
        // TODO: Reuse reader/searcher?
        use reader = DirectoryReader.Open(directory)
        let searcher = IndexSearcher(reader)
        let queryTokens = stopNameToTokens name
        let query = stopNameToQuery queryTokens
        // Ideally we'd override Lucene.Net with a custom scoring method, but
        // that looks complicated, so we take the results the original sorting
        // method gives us and apply our own sorting after that
        searcher.Search(query, null, defaultArg top 10).ScoreDocs
        |> Seq.map (fun sd ->
            let doc = searcher.Doc(sd.Doc)
            let stop = stops[doc.GetField("index").GetInt32Value().Value]
            {
                stop = stop
                score = nameSimilarity queryTokens
                                       (stopNameToTokens <| stop.name)
            })
        |> Seq.toArray

    interface IDisposable with
        member this.Dispose() =
            directory.Dispose()
