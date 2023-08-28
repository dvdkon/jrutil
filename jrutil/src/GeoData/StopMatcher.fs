// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// This module allows matching a stop name to candidates from a list of
/// provided "stops to match". This is useful e.g. for adding locations to
/// stops.
module JrUtil.GeoData.StopMatcher

open System
open System.IO
open Lucene.Net.Util
open Lucene.Net.Store
open Lucene.Net.Documents
open Lucene.Net.Index
open Lucene.Net.Search
open Lucene.Net.Analysis
open Lucene.Net.Analysis.Core
open Lucene.Net.Analysis.Synonym
open Lucene.Net.Analysis.TokenAttributes

[<Struct>]
type StopToMatch<'d> = {
    name: string
    data: 'd
}

type StopMatch<'d> = {
    stop: StopToMatch<'d>
    score: float32
}

let synonymsFile = __SOURCE_DIRECTORY__ + "/StopMatcherSynonyms.txt"

let preprocessStopName (name: string) =
    name
     // Make sure successive acronyms are tokenized as separate
     .Replace(".", ". ")
     // Strip punctuation
     .Replace(',', ' ').Replace('-', ' ')
     // Replace non-space spaces
     .Replace('\u00a0', ' ')
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

let analyzeToTokens (analyzer: Analyzer) (field: string) (str: string) =
    let tokens = ResizeArray()
    use ts = analyzer.GetTokenStream(field, str)
    ts.Reset()
    while ts.IncrementToken() do
        if ts.HasAttribute<ICharTermAttribute>() then
            tokens.Add(ts.GetAttribute<ICharTermAttribute>().ToString())
    tokens

let makeAnalyzer luceneVersion =
    let synonymAnalyzer = new WhitespaceAnalyzer(luceneVersion)
    let synonymParser = SolrSynonymParser(false, true, synonymAnalyzer)
    use synonymReader = new StreamReader(synonymsFile)
    synonymParser.Parse(synonymReader)
    Analyzer.NewAnonymous(fun fieldName reader ->
        let tokenizer = new WhitespaceTokenizer(luceneVersion, reader)
        let synonymFilter = new SynonymFilter(
            tokenizer, synonymParser.Build(), true)
        TokenStreamComponents(tokenizer, synonymFilter))

type StopMatcher<'d>(stops: StopToMatch<'d> array) as this =
    let luceneVersion = LuceneVersion.LUCENE_48
    let directory = new RAMDirectory()
    let analyzer = makeAnalyzer luceneVersion
    let mutable cachedReader = None
    do this.index()

    member this.index() =
        let config = IndexWriterConfig(luceneVersion, analyzer)
        use writer = new IndexWriter(directory, config)

        for i, stop in stops |> Seq.indexed do
            let doc = Document()
            doc.Add(new Field("name", preprocessStopName stop.name,
                              TextField.TYPE_NOT_STORED))
            doc.Add(new Int32Field("index", i, Field.Store.YES))
            writer.AddDocument(doc)

    member this.nameSimilarity(queryTokens: string array,
                               matchedTokens: string array) =
        let qtExpandedSet =
            queryTokens
            |> Seq.collect (fun t -> analyzeToTokens analyzer "name" t)
            |> set
        // Result: How many of the matched stop's words are also in the query
        let matchedCount =
            matchedTokens
            |> Array.sumBy (fun mt ->
                let synonyms = analyzeToTokens analyzer "name" mt
                if Set.intersect (set synonyms) qtExpandedSet |> Set.isEmpty |> not
                then 1
                else 0)
        float32 matchedCount / float32 matchedTokens.Length

    member this.reader =
        match cachedReader with
        | Some r -> r
        | None ->
            let r = DirectoryReader.Open(directory)
            cachedReader <- Some r
            r


    member this.matchStop(name, ?top) =
        let searcher = IndexSearcher(this.reader)
        let queryTokens = stopNameToTokens name
        let query = stopNameToQuery queryTokens
        // Ideally we'd override Lucene.Net with a custom scoring method, but
        // that looks complicated, so we take the results the original sorting
        // method gives us and apply our own sorting after that
        searcher.Search(query, null, defaultArg top 100).ScoreDocs
        |> Seq.map (fun sd ->
            let doc = searcher.Doc(sd.Doc)
            let stop = stops[doc.GetField("index").GetInt32Value().Value]
            {
                stop = stop
                score = this.nameSimilarity(
                    queryTokens, (stopNameToTokens <| stop.name))
            })
        |> Seq.toArray

    interface IDisposable with
        member this.Dispose() =
            cachedReader |> Option.iter (fun r -> r.Dispose())
            analyzer.Dispose()
            directory.Dispose()
