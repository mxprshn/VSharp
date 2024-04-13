namespace VSharp.MethodSequences

open System
open System.Reflection
open VSharp
open VSharp.Core

module Utils =

    let hasInstanceThis (method : IMethod) = not method.IsConstructor && method.HasThis
    
    let isPrimitive (t : Type) = t.IsPrimitive || t.IsEnum

    let rec canBeCreatedBySolver (t : Type) =
        isPrimitive t ||
            t = typeof<string> ||
            (Types.IsNullable t && canBeCreatedBySolver <| Nullable.GetUnderlyingType t) ||
            (t.IsArray && canBeCreatedBySolver <| t.GetElementType()) ||
            (t.IsByRef && canBeCreatedBySolver <| t.GetElementType())
            
    let getThisAndParametersKeys (method : IMethod) = seq {
        if hasInstanceThis method then
            yield ThisKey method
        for parameterInfo in method.Parameters do
            yield ParameterKey parameterInfo
    }
    
    let getNonTrivialParametersCount (method : IMethod) : int =
        let nonTrivialParams =
            Seq.map (fun (p : ParameterInfo) -> p.ParameterType) method.Parameters |>
            Seq.filter (canBeCreatedBySolver >> not)
        Seq.length nonTrivialParams
        
    let publicFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.Public ||| BindingFlags.Instance
        
    let getPropertySetters (typ : Type) : IMethod seq =
        typ.GetProperties publicFlags |>
            Seq.map (_.GetSetMethod()) |> // TODO: filter by public
            Seq.filter (fun m -> m <> null) |>
            Seq.map VSharp.Application.getMethod |>
            Seq.cast<IMethod>
            
    let isSetter (method : IMethod): bool =
        // Dirty hack
        method.Name.StartsWith "set_" && method.Parameters.Length = 1
    
    let rec nonEmptySubsets (lst : 'a list) : 'a list list =
        match lst with
        | [] -> [[]]
        | x::xs ->
            let subsets = nonEmptySubsets xs
            subsets @ List.map (fun subset -> x :: subset) subsets
            
    let rec private getFieldsFromConstantRec (constant : ISymbolicConstantSource) (acc : fieldId list) : fieldId list =
        match constant with
        | HeapAddressSource(bs) ->
            getFieldsFromConstantRec bs acc
        | HeapReading(_, _ ) as hr ->
            match GetHeapReadingRegionSort hr with
            | HeapFieldSort(fieldId) ->
                [ fieldId ]
            | _ -> []
        | StructFieldChain(_ :: _ as ids, bs) ->
            getFieldsFromConstantRec bs (ids @ acc)
        | StructFieldSource(_, fieldId) ->
            [ fieldId ]
        | _ -> []
       
    let getFieldsFromConstant (constant : ISymbolicConstantSource) : fieldId list =
        getFieldsFromConstantRec constant []
