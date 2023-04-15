module Main

-- IMustUnify

import public Concurrent.Api.ConcurrentDsl

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
startArgument: Integer
startArgument = 1

lambdaBodyLet: ?
lambdaBodyLet = `(\startArgumentWrapped => do
let result1 = concurrentFunction1 << startArgumentWrapped 
let result2 = concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
result2)

------------------------------------TTImp to Graph parser-------------------------------------------------------------------------------------------


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
                    `rxMapInternalFst` dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1)
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
                    `rxMapInternalFst` dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1)
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

printTestFunctionResult : Show a => IO a -> IO ()
printTestFunctionResult ioX = do
    x <- ioX
    printLn $ show x







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
                    `rxMapInternalFst` dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1)
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

%runElab makeFunctionConcurrent "testMakeFunctionConcurrent" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments

compareWithReferenceAll : (input : Integer) -> (generated : Integer -> IO Integer) -> (reference : Integer -> IO Integer) -> IO Bool
compareWithReferenceAll input generated reference = do
    actual <- generated input
    expected <- reference input
    pure $ actual == expected
----------------------------------------------------------------------------------------------------------------------------------------------------
covering
executeConcurrent: (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
executeConcurrent functionName inputType _ function = do 
    functionBody <- Reflection.quote function
    inputArgType <- Reflection.quote inputType
    let stub : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1)
                    `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
                    `rxMap` (\((graph, (partition, bodies)), inputArg) => composeConcurrentFunctions inputArg functionName partition graph bodies )
                    `rxJoin` () 
                    `rxFlatMap` either (const stub) id

    traverse_ declare $ fst res 





covering
testExecuteConcurrentDebug : (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()--$  List Decl--TTImp--TypedSplittedFunctionBody--Table SplittedFunctionBody--List SplittedFunctionBody --Errorable (List SplittedFunctionBody, ArgumentType) --List TTImp
testExecuteConcurrentDebug functionName inputType outputArgument function = do 
    let localStub : (List $ List Decl, List Decl) = ([], [])
    functionBody <- Reflection.quote function
    outputArgumentType <- Reflection.quote outputArgument
    let errorStubComposeFunctions : (List $ List Decl, List Name) := ([], [])
    let res = parseLambdaFunction functionBody
                    `rxMapInternalFst` List.reverse
                    `rxMapInternalFst` constructDataDependencieGraph
                    `rxMapInternalFst` dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1)
                    `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
                    `rxMap` (\((graph, (partition, bodies)), inputArg) =>  
                                        composeConcurrentFunctions inputArg functionName partition graph bodies
                                        `rxMap` (\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)
                            )
                    `rxJoin` ()
                    `rxMap` (\wrapper => 
                                let init = composeInitializationFunction functionName wrapper.startArgument outputArgumentType wrapper.functionNames wrapper.graph
                                in (wrapper.functionDeclarations, init)
                        )
                    `rxJoinEitherPair` ()
                    `rxFlatMap` either (const localStub) id

    traverse_ declare $ fst res
    declare $ snd res







-- testExecuteConcurrentObj : ?
-- testExecuteConcurrentObj = 
%runElab makeFunctionConcurrent "concurrentFunction" Integer Integer $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments
------------------------------------------- Legacy Api ----------------------------------------------------------------------
getFunctionBodyAndArguments : TTImp -> (Maybe Arg, TTImp)
getFunctionBodyAndArguments = mapFst fst . unLambda

-- covering
-- createConcurrentExecution: (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
-- createConcurrentExecution function = do
--     baseFunctionName <- genSym "concurrentFunctionBaseName"
--     functionTTImp <- Reflection.quote function
--     let (maybeArg, body) = getFunctionBodyAndArguments functionTTImp
--     let functions = splitFunctionByMonoidArrow body
--     let dataDependencieGraph = constructDataDependencieGraph functions
--     let partition = simplifyDependencies $ doBiPartition RandomBiPartitioner WeightAll1 dataDependencieGraph
--     let simplifyedPartition = simplifyDependencies partition
--     let functionsBodies : List TTImp :=  mapT generateFunctionBody simplifyedPartition
--     let (declarations, names) = composeFunctions ?argType ?baseFunctionName partition dataDependencieGraph functionsBodies
--     foldl (\_, decl => do declare decl) (pure ()) declarations
----------------------------------------------------------------------------------------------------------------------------------------------------






















-------------------------------- Debug --------------------------------------------------------
s1Syntax : Integer -> Integer
s1Syntax param1 = 
    let variable = concurrentFunction1 param1 
    in let variable2 = concurrentFunction2 variable
    in concurrentFunction3 variable2

s1 : ?
s1 = `(let variable = concurrentFunction1 param1 
    in let variable2 = concurrentFunction2 variable
    in concurrentFunction3 variable2)


sIO : Channel Integer -> Channel Integer -> Integer -> IO ()
sIO resultChannel depChannel param1 = do
    let variable = concurrentFunction1 param1 
    channelPut depChannel variable
    let variable2 = concurrentFunction2 variable
    channelPut resultChannel $ concurrentFunction3 variable2

sIO' : ?
sIO' = `(do
    let variable = concurrentFunction1 param1 
    channelPut depChannel variable
    let variable2 = concurrentFunction2 variable
    channelPut resultChannel $ concurrentFunction3 variable2)

sIO1' : ?
sIO1' = `(do
    channelPut depChannel param1)

sIO2' : ?
sIO2' = `(channelPut)





toTTImp : (ConcurrentWrap a -> ConcurrentWrap a) -> Elab TTImp
toTTImp f = do Reflection.quote f


sIO3 : ?
sIO3 = firstOrDefault `(stub) $ snd $ unApp $ snd $ getFunctionBodyAndArguments $ %runElab toTTImp $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments








data TestType : Type where
    MkTestType : Nat -> TestType

testImpl : TestType -> TestType -> TestType
testImpl (MkTestType a) (MkTestType b) = MkTestType $ a + b

concatArgumentsTyped: TestType -> TestType -> TestType
concatArgumentsTyped = testImpl

concurrentFunction1Typed: TestType -> TestType
concurrentFunction1Typed = testImpl $ MkTestType 1

concurrentFunction2Typed: TestType -> TestType
concurrentFunction2Typed = testImpl $ MkTestType 2

concurrentFunction3Typed: TestType -> TestType
concurrentFunction3Typed = testImpl $ MkTestType 3

concurrentFunction4Typed: TestType -> TestType
concurrentFunction4Typed = testImpl $ MkTestType 4













toTTImpTyped : (ConcurrentWrap TestType -> ConcurrentWrap TestType) -> Elab TTImp
toTTImpTyped f = do Reflection.quote f

partial 
typeFromArg : List Arg -> TTImp
typeFromArg [MkArg _ _ _ type] = firstOrDefault type $ snd $ unApp type

sIO5Typed : ?
sIO5Typed = %runElab toTTImpTyped $ \saw =>
    let result1 = concurrentFunction1Typed << saw
    in let result2 = concurrentFunction2Typed << concat1 saw result1 concatArgumentsTyped
    in let result3 = concurrentFunction3Typed<< result2
    in concurrentFunction4Typed << concat1 result1 result3 concatArgumentsTyped

sIO5 : ?
sIO5 = `(\saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments)

sIO5' : ?
sIO5' = parseLambdaFunction $ %runElab Reflection.quote $ \saw =>
    let result1 = concurrentFunction1 << saw
    in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
    in let result3 = concurrentFunction3 << result2
    in concurrentFunction4 << concat1 result1 result3 concatArguments













concFunc1 : Channel Nat -> Channel Nat -> Channel Nat -> Channel Nat -> Nat -> IO ()
concFunc1 _ _ _ _ a = do putStr $ show $ a + 1

concFunc2 : Channel Nat -> Channel Nat -> Channel Nat -> Channel Nat -> Nat -> IO ()
concFunc2 _ _ _ _ a = do putStr $ show $ a + 2

concFunc1' : Channel Nat -> Channel Nat -> Channel Nat -> Channel Nat -> Nat -> IO ()
concFunc1' c1 c2 c3 c4 a = do
    let startValue = a + 1
    v1 <- putStr $ show startValue
    v2 <- channelPut c1 startValue
    c2Value <- channelGet c2
    putStr $ show c2Value

concFunc2' : Channel Nat -> Channel Nat -> Channel Nat -> Channel Nat -> Nat -> IO ()
concFunc2' c1 c2 c3 c4 a = do
    v2 <- putStr $ show a
    c1Value <- channelGet c1
    v2 <- putStr $ show $ c1Value + a
    v3 <- channelPut c2 $ c1Value + a
    channelPut c4 $ c1Value + a

cTest1 : Channel Nat -> Channel Nat -> Channel Nat -> Channel Nat -> Nat -> IO ()
cTest1 c1 chan2 c3 c4 a = do
    let startValue = a + 1
    v1 <- channelPut c1 startValue
    c2Value <- channelGet chan2
    putStrLn $ show $ startValue + c2Value

cTest2 : Channel Nat -> Channel Nat -> Channel Nat -> Channel Nat -> Nat -> IO ()
cTest2 c1 c2 c3 c4 a = do
    c1Value <- channelGet c1
    putStrLn $ show $ c1Value + a
    channelPut c2 $ c1Value + a










testFunc1 : Nat -> IO ()
testFunc1 inputArgument = do
    channel1 <- makeChannel 
    channel2 <- makeChannel
    channel3 <- makeChannel
    channel4 <- makeChannel
    let func1 = concFunc1 channel1 channel2 channel3 channel4
    let func2 = concFunc2 channel1 channel2 channel3 channel4
    runConcurrent inputArgument [func1, func2]

testFunc2 : Nat -> IO ()
testFunc2 inputArgument = do
    channel1 <- makeChannel 
    channel2 <- makeChannel
    channel3 <- makeChannel
    channel4 <- makeChannel
    let func1 = cTest1 channel1 channel2 channel3 channel4
    let func2 = cTest2 channel1 channel2 channel3 channel4
    runConcurrentFork' channel4 inputArgument [func1, func2]

testFunc3 : Nat -> IO ()
testFunc3 a = do 
    channel1 <- makeChannel {a=Nat}
    channel2 <- makeChannel
    channel3 <- makeChannel
    channel4 <- makeChannel
    id <- fork $ cTest1 channel1 channel2 channel3 channel4 a
    channelPut channel2 a
    c1Value <- channelGet channel1
    putStrLn $ show $ c1Value

testFunc4 : Nat -> IO Nat
testFunc4 a = do 
    channel1 <- makeChannel 
    channel2 <- makeChannel
    channel3 <- makeChannel
    channel4 <- makeChannel
    id1 <- fork $ concFunc1' channel1 channel2 channel3 channel4 a
    id2 <- fork $ concFunc2' channel1 channel2 channel3 channel4 a
    channelGet channel4







check : (a : Type) -> (b : Type) -> (a -> IO b) -> Elab TTImp
check _ _ f = do Reflection.quote f

initFunctionExample : ?
initFunctionExample = %runElab check Nat Nat $ \inputArgument => do
    channel1 <- makeChannel {a=Nat}
    channel2 <- makeChannel {a=Nat}
    channel3 <- makeChannel {a=Nat}
    channel4 <- makeChannel {a=Nat}
    func1 <- fork $ cTest1 channel1 channel2 channel3 channel4 inputArgument
    func2 <- fork $ cTest2 channel1 channel2 channel3 channel4 inputArgument
    channelGet channel4

initFunctionExampleTypeUnspec : ?
initFunctionExampleTypeUnspec = %runElab check Nat Nat $ \inputArgument => do
    channel1 <- makeChannel --{a=Nat}
    channel2 <- makeChannel --{a=Nat}
    channel3 <- makeChannel --{a=Nat}
    channel4 <- makeChannel --{a=Nat}
    func1 <- fork $ cTest1 channel1 channel2 channel3 channel4 inputArgument
    func2 <- fork $ cTest2 channel1 channel2 channel3 channel4 inputArgument
    channelGet channel4











testFunctioIOGeneric : Integer -> Integer -> IO ()
testFunctioIOGeneric offset base = do printLn $ base + offset

testFunctionList : List $ Integer -> IO ()
testFunctionList = map testFunctioIOGeneric [1,2,3,4,5,6,7,8,9,10]

---------------------------------------------------------------------------------------------------------------------------