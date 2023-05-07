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

---------------------------------------------------------------EXAMPLE----------------------------------------------------------------

BaseType1: Type
BaseType1 = Integer

BaseType2: Type
BaseType2 = Integer

BaseType3: Type
BaseType3 = Integer

-->>>>>>>>>>>>>>>>>>>functions<<<<<<<<<<<<<<<<<<<<<<--
concatArguments: Integer -> Integer -> Integer
concatArguments = (+)

concurrentFunction1: Integer -> Integer
concurrentFunction1 = (+) 1

concurrentFunction2: Integer -> Integer
concurrentFunction2 = (+) 2

concurrentFunction3: Integer -> Integer
concurrentFunction3 = (+) 3

concurrentFunction4: Integer -> Integer
concurrentFunction4 = (+) 4

concurrentFunction5: Integer -> Integer
concurrentFunction5 = (+) 5

concurrentFunction6: Integer -> Integer
concurrentFunction6 = (+) 6

concurrentFunction7: Integer -> Integer
concurrentFunction7 = (+) 7
-->>>>>>>>>>>>>>>>>>>functions<<<<<<<<<<<<<<<<<<<<<<--


------------------------------------ Tests -------------------------------------------------------------------------------------------
-- Должен создавать деклорации базовых паралельных функций
covering
testGenerateConcurrentFunctionsDeclarations : (concurrentFunctionNameBase : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()--TypedSplittedFunctionBody--Table SplittedFunctionBody--List SplittedFunctionBody --Errorable (List SplittedFunctionBody, ArgumentType) --List TTImp
testGenerateConcurrentFunctionsDeclarations concurrentFunctionNameBase inputType _ function = do 
    let localStub : List Decl = []
    functionBody <- Reflection.quote function
    inputArgType <- Reflection.quote inputType
    let stub : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . findGraphPartition BisectionPartitioner WeightAll1)
                    `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
                    `rxMap` (\((graph, (partition, bodies)), inputArgument) => do
                                let names = generateNames partition concurrentFunctionNameBase
                                let functions = map var names
                                joinListEither $ map (generateFunctionDeclaration inputArgument.type graph) names
                            )
                    `rxJoin` () 
                    `rxFlatMap` either (const localStub) id

    declare res

-- check manual :s Channel Integer -> Channel Integer -> Channel Integer -> Channel Integer -> Integer -> IO ()
%runElab testGenerateConcurrentFunctionsDeclarations "testFunctionDeclaration" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments

checkDeclarationFunctionExist1 : Channel Integer -> Channel Integer -> Channel Integer -> Channel Integer -> Integer -> IO ()
checkDeclarationFunctionExist1 = testFunctionDeclaration_concurrent_function_1
checkDeclarationFunctionExist2 : Channel Integer -> Channel Integer -> Channel Integer -> Channel Integer -> Integer -> IO ()
checkDeclarationFunctionExist2 = testFunctionDeclaration_concurrent_function_2






covering
testGenerateConcurrentFunctions: (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
testGenerateConcurrentFunctions functionName inputType _ function = do 
    functionBody <- Reflection.quote function
    inputArgType <- Reflection.quote inputType
    let stub : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . findGraphPartition BisectionPartitioner WeightAll1)
                    `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
                    `rxMap` (\((graph, (partition, bodies)), inputArg) => composeConcurrentFunctions inputArg functionName partition graph bodies )
                    `rxJoin` () 
                    `rxFlatMap` either (const stub) id

    traverse_ declare $ fst res 

-- check manual :s Channel Integer -> Channel Integer -> Channel Integer -> Channel Integer -> Integer -> IO ()
%runElab testGenerateConcurrentFunctions "testFunctionGeneration" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments


checkGeneratedFunctionExist1 : Channel Integer -> Channel Integer -> Channel Integer -> Channel Integer -> Integer -> IO ()
checkGeneratedFunctionExist1 = testFunctionGeneration_concurrent_function_1
checkGeneratedFunctionExist2 : Channel Integer -> Channel Integer -> Channel Integer -> Channel Integer -> Integer -> IO ()
checkGeneratedFunctionExist2 = testFunctionGeneration_concurrent_function_2

referenceFunction : Integer -> Integer
referenceFunction saw = 
    let result1 = concurrentFunction1 saw
    in let result2 = concurrentFunction2 $ concatArguments saw result1 
    in let result3 = concurrentFunction3 result2
    in concurrentFunction4 $ concatArguments result1 result3 

checkGeneratedFunctionExec : Integer -> IO Integer
checkGeneratedFunctionExec input = do 
    channel1 <- makeChannel
    channel2 <- makeChannel
    channel3 <- makeChannel
    channel4 <- makeChannel
    id1 <- fork $ testFunctionGeneration_concurrent_function_1 channel1 channel2 channel3 channel4 input
    id2 <- fork $ testFunctionGeneration_concurrent_function_2 channel1 channel2 channel3 channel4 input 
    channelGet channel4

compareWithReference : Integer -> IO Bool
compareWithReference input = do
    let expected = referenceFunction input
    actual <- checkGeneratedFunctionExec input
    pure $ actual == expected







covering
testGenerateInitFunctionDeclaration : (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
testGenerateInitFunctionDeclaration functionName inputType outputArgument function = do 
    let localStub : List Decl = []
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let errorStubComposeFunctions : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . findGraphPartition BisectionPartitioner WeightAll1)
                    `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
                    `rxMap` (\((graph, (partition, bodies)), inputArg) =>  
                                        composeConcurrentFunctions inputArg functionName partition graph bodies
                                        `rxMap` (\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)
                            )
                    `rxJoin` ()
                    `rxMap` (\wrapper => generateInitializationFunctionDeclaration functionName wrapper.startArgument outputArgumentType)
                    `rxFlatMap` either (const localStub) wrap

    declare res


%runElab testGenerateInitFunctionDeclaration "testInitFunctionDeclaration" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments


checkDeclarationInitFunctionExist : Integer -> IO Integer
checkDeclarationInitFunctionExist = testInitFunctionDeclaration

%runElab makeFunctionConcurrent BisectionPartitioner "testMakeFunctionConcurrentBI" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments

%runElab makeFunctionConcurrent (KerniganLinParitioner 10 5) "testMakeFunctionConcurrentKL" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments










printIO : Show a => IO a -> IO ()
printIO ioX = do
    x <- ioX
    printLn $ show x

compareWithReferenceAll : (input : Integer) -> (generated : Integer -> IO Integer) -> (reference : Integer -> IO Integer) -> IO Bool
compareWithReferenceAll input generated reference = do
    actual <- generated input
    expected <- reference input
    pure $ actual == expected





covering
executeConcurrent: (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
executeConcurrent functionName inputType _ function = do 
    functionBody <- Reflection.quote function
    inputArgType <- Reflection.quote inputType
    let stub : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . findGraphPartition BisectionPartitioner WeightAll1)
                    `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
                    `rxMap` (\((graph, (partition, bodies)), inputArg) => composeConcurrentFunctions inputArg functionName partition graph bodies )
                    `rxJoin` () 
                    `rxFlatMap` either (const stub) id

    traverse_ declare $ fst res 