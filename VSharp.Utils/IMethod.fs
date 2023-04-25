namespace VSharp

open System
open System.Collections.Generic

type IMethod =
    inherit IComparable
    abstract Name : string
    abstract FullName : string
    abstract ReturnType : Type
    abstract DeclaringType : Type
    abstract ReflectedType : Type
    abstract Parameters : Reflection.ParameterInfo[]
    abstract LocalVariables : IList<Reflection.LocalVariableInfo>
    abstract HasThis : bool
    abstract IsConstructor : bool
    abstract GenericArguments : Type[]
    abstract SubstituteTypeVariables : (Type -> Type) -> IMethod
    abstract MethodBase : System.Reflection.MethodBase
    abstract MetadataToken : int
    abstract IsVirtual : bool
