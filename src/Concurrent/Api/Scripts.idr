module Concurrent.Api.Scripts

import public Concurrent.Parser.LetParser.Utils
import public Concurrent.Parser.LetParser.Api

import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Types.DataFlowGraph

import public Concurrent.Parser.Splitter
import public Concurrent.Parser.LetSplitter
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
makeFunctionConcurrent : (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
makeFunctionConcurrent functionName inputType outputArgument function = do 
    let localStub : (List $ List Decl, List Decl) = ([], [])
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let errorStubComposeFunctions : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1)
                    `rxMapInternalFst` (uncurry generateFunctionBodies)
                    `rxMap` (uncurry4 composeConcurrentFunctionsAndWrap)
                    `rxJoin` ()
                    `rxMap` composeInitializationFunctionAndWrap outputArgumentType
                    `rxJoinEitherPair` ()
                    `rxFlatMap` either (const localStub) id

    traverse_ declare $ fst res
    declare $ snd res where

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
