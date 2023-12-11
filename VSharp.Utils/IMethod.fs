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
    abstract IsExternalMethod : bool
    abstract Parameters : Reflection.ParameterInfo[]
    abstract LocalVariables : IList<Reflection.LocalVariableInfo>
    abstract HasThis : bool
    abstract IsConstructor : bool
    abstract GenericArguments : Type[]
    abstract ContainsGenericParameters : bool
    abstract SubstituteTypeVariables : (Type -> Type) -> IMethod
    abstract MethodBase : System.Reflection.MethodBase
    abstract MetadataToken : int
    abstract IsVirtual : bool
    abstract ResolveOverrideInType : Type -> IMethod
    abstract CanBeOverriddenInType : Type -> bool
    abstract IsImplementedInType : Type -> bool
