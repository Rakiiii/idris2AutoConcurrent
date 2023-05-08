module Concurrent.Api.Scripts

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


-------------------- Scripts -----------------------------------------------------------------
public export 
makeFunctionConcurrent : 
                                  (partitioner : Partitioner TrivaialGraphWeightConfig) ->
                                  (functionName : String)                               -> 
                                  (a : Type)                                            -> 
                                  (b : Type)                                            -> 
                                  (ConcurrentWrap a -> ConcurrentWrap b)                -> 
                                    Elab ()
makeFunctionConcurrent partitioner functionName inputType outputArgument function = do 
    let typedStub : (List $ List Decl, List Decl) = ([], [])
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . findGraphPartition partitioner WeightAll1)
                    `rxMapInternalFst` (uncurry generateFunctionBodies)
                    `rxMap` (uncurrySpecial composeConcurrentFunctionsAndWrap)
                    `rxJoin` ()
                    `rxMap` composeInitializationFunctionAndWrap outputArgumentType
                    `rxJoinEitherPair` ()
                    `rxFlatMap` either (const typedStub) id

    -- logMsg "decl" 0 "before first decl"
    -- logMsg "decl" 0 ""
    -- logMsg "decl" 0 "------------------------------------------------------------------------------------------------------------"
    -- logMsg "decl" 0 ""
    -- for_ (join $ fst res) $ \x => do 
    --     logMsg "decl" 0 ("delarin " ++ show x)
    --     logMsg "decl" 0 ""
    --     logMsg "decl" 0 "------------------------------------------------------------------------------------------------------------"
    --     logMsg "decl" 0 ""
    --     declare $ pure x
    -- logMsg "decl" 0 "after first decl"
    -- logMsg "decl" 0 ""
    -- logMsg "decl" 0 "------------------------------------------------------------------------------------------------------------"
    -- logMsg "decl" 0 ""
    -- declare $ snd res
    -- logMsg "decl" 0 "after second decl"
    -- logMsg "decl" 0 ""
    -- logMsg "decl" 0 "------------------------------------------------------------------------------------------------------------"
    -- logMsg "decl" 0 ""
    logMsg "decl" 0 "before decl concat"
    logMsg "decl" 0 ""
    logMsg "decl" 0 "------------------------------------------------------------------------------------------------------------"
    logMsg "decl" 0 ""

    let listOfDecl = (join $ fst res) ++ snd res
    -- logMsg "decl" 0 ("declaring " ++ show listOfDecl)
    logMsg "decl" 0 "after decl concat"
    logMsg "decl" 0 ""
    logMsg "decl" 0 "------------------------------------------------------------------------------------------------------------"
    logMsg "decl" 0 ""
    declare listOfDecl
    where



    generateFunctionBodies : (graph : Table $ DependentLine TypedSplittedFunctionBody) -> 
                             (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) -> 
                             (
                                Table $ DependentLine TypedSplittedFunctionBody, 
                                GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody, 
                                List TTImp
                             )
    generateFunctionBodies graph partition = 
        let functionsBodies : List TTImp := mapT generateFunctionBody partition 
        in (graph, partition, functionsBodies)



    composeConcurrentFunctionsAndWrap : (graph : Table $ DependentLine TypedSplittedFunctionBody) ->
                                        (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) ->
                                        (bodies : List TTImp) -> 
                                        (inputArg : InputArgument) ->
                                        Errorable ArgumentWrapper
    composeConcurrentFunctionsAndWrap graph partition bodies inputArg = 
        composeConcurrentFunctions inputArg functionName partition graph bodies
            `rxMap` (\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)



    composeInitializationFunctionAndWrap : TTImp -> ArgumentWrapper -> (List $ List Decl, ErrorableList Decl) 
    composeInitializationFunctionAndWrap outputArgumentType wrapper = 
        let init = composeInitializationFunction functionName wrapper.startArgument outputArgumentType wrapper.functionNames wrapper.graph
        in (wrapper.functionDeclarations, init)



    uncurry4 : (a -> b -> c -> d -> e) -> ((a, (b, c)), d) -> e
    uncurry4 f ((a, (b, c)), d) = f a b c d

    uncurrySpecial : (Table GraphLine -> GraphBiPartition OrderedGraphLine -> List TTImp -> InputArgument -> Either Error ArgumentWrapper) -> ((Table GraphLine, (GraphBiPartition OrderedGraphLine, List TTImp)), InputArgument) -> Either Error ArgumentWrapper
    uncurrySpecial f ((a, (b, c)), d) = f a b c d 




















public export 
makeFunctionConcurrent' : 
                                  (partitioner : Partitioner TrivaialGraphWeightConfig)          ->
                                  (functionName : String)                -> 
                                  (a : Type)                             -> 
                                  (b : Type)                             -> 
                                  (ConcurrentWrap a -> ConcurrentWrap b) -> 
                                    -- Elab (List $ List Decl, List Decl)
                                    Elab $ List Decl
makeFunctionConcurrent' partitioner functionName inputType outputArgument function = do 
    let typedStub : (List $ List Decl, List Decl) = ([], [])
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . findGraphPartition partitioner WeightAll1)
                    `rxMapInternalFst` (uncurry generateFunctionBodies)
                    `rxMap` (uncurrySpecial composeConcurrentFunctionsAndWrap)
                    `rxJoin` ()
                    `rxMap` composeInitializationFunctionAndWrap outputArgumentType
                    `rxJoinEitherPair` ()
                    `rxFlatMap` either (const typedStub) id

    -- pure (fst res, snd res)
    pure $ (join $ fst res) ++ snd res
    where



    generateFunctionBodies : (graph : Table $ DependentLine TypedSplittedFunctionBody) -> 
                             (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) -> 
                             (
                                Table $ DependentLine TypedSplittedFunctionBody, 
                                GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody, 
                                List TTImp
                             )
    generateFunctionBodies graph partition = 
        let functionsBodies : List TTImp := mapT generateFunctionBody partition 
        in (graph, partition, functionsBodies)



    composeConcurrentFunctionsAndWrap : (graph : Table $ DependentLine TypedSplittedFunctionBody) ->
                                        (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) ->
                                        (bodies : List TTImp) -> 
                                        (inputArg : InputArgument) ->
                                        Errorable ArgumentWrapper
    composeConcurrentFunctionsAndWrap graph partition bodies inputArg = 
        composeConcurrentFunctions inputArg functionName partition graph bodies
            `rxMap` (\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)



    composeInitializationFunctionAndWrap : TTImp -> ArgumentWrapper -> (List $ List Decl, ErrorableList Decl) 
    composeInitializationFunctionAndWrap outputArgumentType wrapper = 
        let init = composeInitializationFunction functionName wrapper.startArgument outputArgumentType wrapper.functionNames wrapper.graph
        in (wrapper.functionDeclarations, init)



    uncurry4 : (a -> b -> c -> d -> e) -> ((a, (b, c)), d) -> e
    uncurry4 f ((a, (b, c)), d) = f a b c d

    uncurrySpecial : (Table GraphLine -> GraphBiPartition OrderedGraphLine -> List TTImp -> InputArgument -> Either Error ArgumentWrapper) -> ((Table GraphLine, (GraphBiPartition OrderedGraphLine, List TTImp)), InputArgument) -> Either Error ArgumentWrapper
    uncurrySpecial f ((a, (b, c)), d) = f a b c d








public export 
makeFunctionConcurrentSimplePartition : 
                                  (partitioner : Partitioner TrivaialGraphWeightConfig)          ->
                                  (functionName : String)                -> 
                                  (a : Type)                             -> 
                                  (b : Type)                             -> 
                                  (ConcurrentWrap a -> ConcurrentWrap b) -> 
                                    Elab $ GraphBiPartition OrderedGraphLine
makeFunctionConcurrentSimplePartition partitioner functionName inputType outputArgument function = do 
    let typedStub : GraphBiPartition OrderedGraphLine = MkGraphBiPartition (MkTable []) (MkTable [])
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` (simplifyDependencies . findGraphPartition partitioner WeightAll1)
                    `rxFlatMap` either (const typedStub) fst

    pure res
    where



    generateFunctionBodies : (graph : Table $ DependentLine TypedSplittedFunctionBody) -> 
                             (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) -> 
                             (
                                Table $ DependentLine TypedSplittedFunctionBody, 
                                GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody, 
                                List TTImp
                             )
    generateFunctionBodies graph partition = 
        let functionsBodies : List TTImp := mapT generateFunctionBody partition 
        in (graph, partition, functionsBodies)



    composeConcurrentFunctionsAndWrap : (graph : Table $ DependentLine TypedSplittedFunctionBody) ->
                                        (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) ->
                                        (bodies : List TTImp) -> 
                                        (inputArg : InputArgument) ->
                                        Errorable ArgumentWrapper
    composeConcurrentFunctionsAndWrap graph partition bodies inputArg = 
        composeConcurrentFunctions inputArg functionName partition graph bodies
            `rxMap` (\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)



    composeInitializationFunctionAndWrap : TTImp -> ArgumentWrapper -> (List $ List Decl, ErrorableList Decl) 
    composeInitializationFunctionAndWrap outputArgumentType wrapper = 
        let init = composeInitializationFunction functionName wrapper.startArgument outputArgumentType wrapper.functionNames wrapper.graph
        in (wrapper.functionDeclarations, init)



    uncurry4 : (a -> b -> c -> d -> e) -> ((a, (b, c)), d) -> e
    uncurry4 f ((a, (b, c)), d) = f a b c d

    uncurrySpecial : (Table GraphLine -> GraphBiPartition OrderedGraphLine -> List TTImp -> InputArgument -> Either Error ArgumentWrapper) -> ((Table GraphLine, (GraphBiPartition OrderedGraphLine, List TTImp)), InputArgument) -> Either Error ArgumentWrapper
    uncurrySpecial f ((a, (b, c)), d) = f a b c d


















public export 
makeFunctionConcurrentOriginalPartition : 
                                  (partitioner : Partitioner TrivaialGraphWeightConfig)          ->
                                  (functionName : String)                -> 
                                  (a : Type)                             -> 
                                  (b : Type)                             -> 
                                  (ConcurrentWrap a -> ConcurrentWrap b) -> 
                                    Elab $ GraphBiPartition OrderedGraphLine
makeFunctionConcurrentOriginalPartition partitioner functionName inputType outputArgument function = do 
    let typedStub : GraphBiPartition OrderedGraphLine = MkGraphBiPartition (MkTable []) (MkTable [])
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` (findGraphPartition partitioner WeightAll1)
                    `rxFlatMap` either (const typedStub) fst

    pure res
    where



    generateFunctionBodies : (graph : Table $ DependentLine TypedSplittedFunctionBody) -> 
                             (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) -> 
                             (
                                Table $ DependentLine TypedSplittedFunctionBody, 
                                GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody, 
                                List TTImp
                             )
    generateFunctionBodies graph partition = 
        let functionsBodies : List TTImp := mapT generateFunctionBody partition 
        in (graph, partition, functionsBodies)



    composeConcurrentFunctionsAndWrap : (graph : Table $ DependentLine TypedSplittedFunctionBody) ->
                                        (partition : GraphBiPartition $ OrderedDependentLine TypedSplittedFunctionBody) ->
                                        (bodies : List TTImp) -> 
                                        (inputArg : InputArgument) ->
                                        Errorable ArgumentWrapper
    composeConcurrentFunctionsAndWrap graph partition bodies inputArg = 
        composeConcurrentFunctions inputArg functionName partition graph bodies
            `rxMap` (\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)



    composeInitializationFunctionAndWrap : TTImp -> ArgumentWrapper -> (List $ List Decl, ErrorableList Decl) 
    composeInitializationFunctionAndWrap outputArgumentType wrapper = 
        let init = composeInitializationFunction functionName wrapper.startArgument outputArgumentType wrapper.functionNames wrapper.graph
        in (wrapper.functionDeclarations, init)



    uncurry4 : (a -> b -> c -> d -> e) -> ((a, (b, c)), d) -> e
    uncurry4 f ((a, (b, c)), d) = f a b c d

    uncurrySpecial : (Table GraphLine -> GraphBiPartition OrderedGraphLine -> List TTImp -> InputArgument -> Either Error ArgumentWrapper) -> ((Table GraphLine, (GraphBiPartition OrderedGraphLine, List TTImp)), InputArgument) -> Either Error ArgumentWrapper
    uncurrySpecial f ((a, (b, c)), d) = f a b c d


public export 
makeFunctionConcurrentBody : 
                                  (partitioner : Partitioner TrivaialGraphWeightConfig)          ->
                                  (functionName : String)                -> 
                                  (a : Type)                             -> 
                                  (b : Type)                             -> 
                                  (ConcurrentWrap a -> ConcurrentWrap b) -> 
                                    Elab TTImp
makeFunctionConcurrentBody partitioner functionName inputType outputArgument function = do 
    Reflection.quote function