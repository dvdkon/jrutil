// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2018 David Koňařík

module JrUtil.ReflectionUtils

open System
open Microsoft.FSharp.Reflection

open JrUtil.Utils

let typeIsOption (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>

let optionCaseGetter =
    FSharpValue.PreComputeUnionTagReader(typedefof<_ option>)

let getOptionCases = memoize <| fun (optType: Type) ->
    let optionCases = FSharpType.GetUnionCases(optType)
    let someCase = optionCases |> Array.find (fun c -> c.Name = "Some")
    let noneCase = optionCases |> Array.find (fun c -> c.Name = "None")
    (someCase, noneCase)

let getOptionConstructors = memoize <| fun (optType: Type) ->
    let someCase, noneCase = getOptionCases optType
    let someCtor = FSharpValue.PreComputeUnionConstructor(someCase)
    let noneCtor = FSharpValue.PreComputeUnionConstructor(noneCase)
    (someCtor, noneCtor)

let optionSomeCase =
    FSharpType.GetUnionCases(typedefof<_ option>)
    |> Array.find (fun uc -> uc.Name = "Some")