namespace VSharp

open System
open System.IO
open System.Xml.Serialization

[<CLIMutable>]
[<Serializable>]
type pathReplay = {
    assemblyPath : string
    moduleFullyQualifiedName : string
    methodToken : int32
    forkIndices : int32 array
}
with
    member x.Serialize (path : string) =
        let serializer = XmlSerializer typeof<pathReplay>
        use stream = File.Create path
        serializer.Serialize(stream, x)
        

