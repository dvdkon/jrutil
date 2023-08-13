// This script finds JDF batches that don't contain town names, only reduced
// stop names used in MHD.
#load "jrutil/ref.fsx"
open JrUtil

let stopsParser = Jdf.fileParser "Zastavky.txt"

JrUtil.Jdf.findJdfBatches fsi.CommandLineArgs.[1]
|> Seq.filter (fun b ->
    let stops: JdfModel.Stop array = stopsParser b
    stops
    |> Array.map (fun s -> s.district.IsNone && s.nearbyPlace.IsNone)
    |> Array.contains false |> not)
|> Seq.iter (printfn "%s")
