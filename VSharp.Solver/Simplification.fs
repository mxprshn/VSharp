namespace VSharp.Solver

open Microsoft.Z3
open System.Collections.Generic
open VSharp
open Logger

module public Simplification =

    // Rewrite all with CPS
    // Maybe node is not needed at all? Can't we work with BoolExprs directly?
    // Rewrite for directed acyclic graphs

    type private Node =
        | Empty
        | Leaf of BoolExpr
        | Conjunction of Node seq
        | Disjunction of Node seq 

    type private Graph = Node

    type private Redundancy =
        | NonConstraining
        | NonRelaxing
        | NotRedundant

    type Simplifier(context : Context) =

        let solver = context.MkSolver()

        let rec boolExprToGraph (cache : Dictionary<BoolExpr, Node>) (expr : BoolExpr) : Graph =
            if expr.IsAnd then
                let args = seq expr.Args |> Seq.cast<BoolExpr> |> Seq.map (boolExprToGraph cache)
                Conjunction(Seq.toList args)
            elif expr.IsOr then
                let args = seq expr.Args |> Seq.cast<BoolExpr> |> Seq.map (boolExprToGraph cache)
                Disjunction(Seq.toList args)
            else
                let mutable leaf = Empty
                if cache.TryGetValue(expr, &leaf) then
                    leaf
                else
                    leaf <- Leaf(expr)
                    cache.[expr] <- leaf
                    leaf

        let rec graphToBoolExpr graph =
            match graph with
            | Empty -> context.MkTrue()
            | Leaf expr -> expr 
            | Conjunction args -> args |> Seq.map graphToBoolExpr |> context.MkAnd 
            | Disjunction args -> args |> Seq.map graphToBoolExpr |> context.MkOr

        let checkRedundancy leaf critical =
            let implication = context.MkAnd(graphToBoolExpr critical,  context.MkNot(graphToBoolExpr leaf))
            //printLog Trace $"Check-sat: {implication}"
            match solver.Check implication with
            | Status.UNSATISFIABLE ->
                //printLog Trace "Sat, non-constraining"
                NonConstraining
            | _ ->
                let implication = context.MkAnd(graphToBoolExpr critical, graphToBoolExpr leaf)
                //printLog Trace $"Check-sat: {implication}"
                match solver.Check implication with
                | Status.UNSATISFIABLE ->
                    //printLog Trace "Sat, non-relaxing"
                    NonRelaxing
                | _ -> NotRedundant

        let rec not node =
            match node with
            | Empty -> Empty
            | Conjunction args -> args |> Seq.map not |> Disjunction
            | Disjunction args -> args |> Seq.map not |> Conjunction

        let rec simplifyNode node critical =
            match node with
            | Empty -> failwith "Cannot simplify empty node"
            | Leaf _ as leaf->
                match checkRedundancy leaf critical with
                | NonConstraining -> context.MkTrue() |> Leaf
                | NonRelaxing -> context.MkFalse() |> Leaf
                | NotRedundant -> leaf
            | Conjunction children ->
                let rec updateChildren (oldChildren : Node array) (newChildren : Node array) =
                    Array.iteri (fun i oldChild ->
                        let newCritical = Conjunction <| seq {
                            yield critical
                            for j in 0 .. i - 1 -> newChildren[j]
                            for j in i + 1 .. oldChildren.Length - 1 -> oldChildren[j] 
                        } 
                        let newChild = simplifyNode oldChild newCritical 
                        newChildren[i] <- newChild
                    ) oldChildren
                    if Array.forall2 (=) oldChildren newChildren then
                        Conjunction newChildren
                    else
                        updateChildren newChildren oldChildren
                updateChildren (Seq.toArray children) (Seq.toArray children)
            | Disjunction children ->
                let rec updateChildren (oldChildren : Node array) (newChildren : Node array) =
                    Array.iteri (fun i oldChild ->
                        let newCritical = Conjunction <| seq {
                            yield critical
                            for j in 0 .. i - 1 -> not newChildren[j]
                            for j in i + 1 .. oldChildren.Length - 1 -> not oldChildren[j] 
                        } 
                        let newChild = simplifyNode oldChild newCritical 
                        newChildren[i] <- newChild
                    ) oldChildren
                    if Array.forall2 (=) oldChildren newChildren then
                       Disjunction newChildren
                    else
                        updateChildren newChildren oldChildren
                updateChildren (Seq.toArray children) (Seq.toArray children)

        member x.Simplify expr =
            let graph = boolExprToGraph (Dictionary()) expr
            printLog Trace $"Graph: {graph}"
            let simplified = simplifyNode graph (Leaf(context.MkTrue()))
            graphToBoolExpr simplified
