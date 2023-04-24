module Main

import public Concurrent.Api.ConcurrentDsl

import public Concurrent.Parser.LetParser.Utils
import public Concurrent.Parser.LetParser.Api

import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Types.DataFlowGraph

import public Concurrent.Parser.GraphConstructor

import public Concurrent.Partition.GraphPartitioner

import public Concurrent.Generator.FunctionGenerator

import public Concurrent.Scheduler.ConcurrentFunctionScheduler

import public Concurrent.Typer.FunctionTyper

import public  Concurrent.Utils.IR

import public Data.List
import public Data.Zippable
import public Control.Function

import public Language.Reflection
import public Language.Reflection.Derive
import public Language.Reflection.Types
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Text.PrettyPrint.Bernardy

import public System
import public System.Concurrency
import public System.Future

import public Prelude

import public Concurrent.Api.Scripts


%default covering
%language ElabReflection



-- extractResult : (List (KLGraphNodeNonParted graphSize), (Bool, (Nat, Bool))) -> (KLGraphNonParted graphSize, Bool)
-- extractResult (nodes, (_, (_ , isOptimalOrderFound))) = (MkKLGraphNonParted nodes, isOptimalOrderFound)

-- isNodesWithoutOrderExist : KLGraphNonParted graphSize -> Bool
-- isNodesWithoutOrderExist graph = any (((==) 0) . order) graph.nodes

-- checkOrderIsNotZero : (graph : List $ KLGraphNodeNonParted graphSize) -> (connectedNode : Fin graphSize) -> Bool
-- checkOrderIsNotZero []      connectedNode  = True
-- checkOrderIsNotZero (x::xs) connectedNode  = 
--         if x.index == connectedNode 
--          then x.order /= 0 
--          else checkOrderIsNotZero xs connectedNode

-- isAllConnectedNodesMarked : (nodes : List $ KLGraphNodeNonParted graphSize) -> (connectedNodes : List $ Fin graphSize) -> Bool
-- isAllConnectedNodesMarked nodes connectedNodes = all (checkOrderIsNotZero nodes) connectedNodes

-- markNodeWithOrderIfAllConnectedNodeAreMarkedFast : (orderMark : Nat) -> (nodes : List $ KLGraphNodeNonParted graphSize) -> (node : KLGraphNodeNonParted graphSize) -> (acc : (List $ KLGraphNodeNonParted graphSize, Nat)) ->(List $ KLGraphNodeNonParted graphSize, Nat)
-- markNodeWithOrderIfAllConnectedNodeAreMarkedFast orderMark nodes node (acc, counter) = 
--         if node.order == 0 && (isNil node.connectedNodes || isAllConnectedNodesMarked nodes node.connectedNodes)
--          then (({ order := orderMark } node)::acc, increment counter)
--          else (node::acc, counter)

-- addOrderToNodeIfNeededFast : Nat -> List (KLGraphNodeNonParted graphSize) -> (List $ KLGraphNodeNonParted graphSize, Nat)
-- addOrderToNodeIfNeededFast nextOrder nodes = foldr (markNodeWithOrderIfAllConnectedNodeAreMarkedFast nextOrder nodes) ([], 0) nodes

-- tryToAddOptimalOrderByDeadlineFastInternal : (shouldBeModified : Bool) -> (orderOnPrevItter : Nat) -> (maxAmountOfSteps : Nat)-> (List $ KLGraphNodeNonParted graphSize) -> (List $ KLGraphNodeNonParted  graphSize, Bool, Nat, Bool)
-- tryToAddOptimalOrderByDeadlineFastInternal False orderOnPrevItter _                    nodes = (nodes, False, orderOnPrevItter, True)
-- tryToAddOptimalOrderByDeadlineFastInternal True  orderOnPrevItter Z                    nodes = (nodes, False, orderOnPrevItter, False)
-- tryToAddOptimalOrderByDeadlineFastInternal _     orderOnPrevItter (S maxAmountOfSteps) nodes = 
--         let (suborderedGraph, amountOf) = addOrderToNodeIfNeededFast orderOnPrevItter nodes
--         in tryToAddOptimalOrderByDeadlineFastInternal isSomethingForModifyExist (increment orderOnPrevItter) maxAmountOfSteps suborderedGraph

-- public export 
-- tryToAddOptimalOrderByDeadlineFast' : (maxAmountOfSteps : Nat)-> KLGraphNonParted graphSize -> (KLGraphNonParted graphSize, Bool)
-- tryToAddOptimalOrderByDeadlineFast' maxAmountOfSteps graph = extractResult $ tryToAddOptimalOrderByDeadlineFastInternal True 1 maxAmountOfSteps graph.nodes where







grC : KLGraphNonParted 4
grC = MkKLGraphNonParted [
    MkKLGraphNodeNonParted 0 [2]    1 0,
    MkKLGraphNodeNonParted 1 [0]    1 0,
    MkKLGraphNodeNonParted 2 []     1 0,
    MkKLGraphNodeNonParted 3 [1, 2] 1 0
]

gr : KLGraphNonParted 4
gr = MkKLGraphNonParted [
    MkKLGraphNodeNonParted 0 [1, 2] 1 3,
    MkKLGraphNodeNonParted 1 [0, 3] 1 2,
    MkKLGraphNodeNonParted 2 [0, 3] 1 1,
    MkKLGraphNodeNonParted 3 [1, 2] 1 4
]

grS : KLGraph 4
grS = createStartPartition gr

debugKL : Nat -> KLGraphNonParted 4 -> KLGraph 4
debugKL n partition = do 
    let start = createStartPartition partition
    itterateOverKerniganLine n start

    -- ?debugKL_rhs

printDebug : KLGraph a -> (Nat, String)
printDebug g = do
    let ext = div (sum $ map externalRibs g.nodes) 2
    (ext, show g)


-- brokeElab : KLGraphNonParted 4 -> Elab $ KLGraph 4
-- brokeElab graph = do 
--     let (klGraphNonPartedNoCrossRibs, isGraphOptimalOrderWasFound) = tryToAddOptimalOrderByDeadline (length graph.nodes) graph
--     let klGraphNonParted = addCrossRibs klGraphNonPartedNoCrossRibs
--     let startPartition = createStartPartition klGraphNonParted
--     let partition = itterateOverKerniganLine 10 startPartition
--     -- ?rt_rhs
--     pure partition

-- brokeElabInstance : KLGraph 4
-- brokeElabInstance = %runElab brokeElab grC