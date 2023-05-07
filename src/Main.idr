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






-- covering
-- testExecuteConcurrentDebug : (algorithm : PartitionAlgorithms) -> (functionName : String) -> (a : Type) -> (b : Type) -> (ConcurrentWrap a -> ConcurrentWrap b) -> 
--                                 Elab (List $ List Decl, List Decl)--$ Table GraphLine--$ GraphBiPartition OrderedGraphLine--$ List $ List Decl--TTImp--TypedSplittedFunctionBody--Table SplittedFunctionBody--List SplittedFunctionBody --Errorable (List SplittedFunctionBody, ArgumentType) --List TTImp
-- testExecuteConcurrentDebug alg functionName inputType outputArgument function = do 
--     let localStub : Table GraphLine := MkTable []
--     let inStub : InputArgument := MkInputArgument `(STUB) ""
--     let partitioner = algorithm RandomBiPartitioner KLBiPartitioner alg
--     functionBody <- Reflection.quote function
--     outputArgumentType <- Reflection.quote outputArgument
--     let errorStubComposeFunctions : (List $ List Decl, List Name) := ([], [])
--     let res = parseLambdaFunction functionBody
--                     `rxMapInternalFst` List.reverse
--                     `rxMapInternalFst` constructDataDependencieGraph
--                     -- `rxMap` fst
--                     -- `rxMapInternalFst` dup (simplifyDependencies . doBiPartition partitioner WeightAll1)
--                     -- `rxMap` (\((_,r) , _) => r)
--                     -- `rxMapInternalFst` (\(graph, partition) => let functionsBodies : List TTImp := mapT generateFunctionBody partition in (graph, partition, functionsBodies))
--                     -- `rxMap` (\((graph, (partition, bodies)), inputArg) =>  
--                     --                     composeConcurrentFunctions inputArg functionName partition graph bodies
--                     --                     `rxMap` fst--(\pair => MkArgumentWrapper (fst pair) (snd pair) inputArg graph)
--                     --         )
--                     -- `rxJoin` ()
--                     -- `rxMap` (\wrapper => 
--                     --             let init = composeInitializationFunction functionName wrapper.startArgument outputArgumentType wrapper.functionNames wrapper.graph
--                     --             in (wrapper.functionDeclarations, init)
--                     --     )
--                     -- `rxJoinEitherPair` ()
--                     -- `rxFlatMap` either (const localStub) id
--                     `rxFlatMap` either (const (localStub, inStub)) id
--     let weightedGraph = addWeightsToGraph WeightAll1 $ Builtin.fst res
--     let klGraphNonPartedNoCrossRibsNotOrdered = convertToKLGraph weightedGraph
--     let (klGraphNonPartedNoCrossRibs, isGraphOptimalOrderWasFound) = tryToAddOptimalOrderByDeadline 4 klGraphNonPartedNoCrossRibsNotOrdered--(length klGraphNonPartedNoCrossRibsNotOrdered.nodes) klGraphNonPartedNoCrossRibsNotOrdered
--     let klGraphNonParted = addCrossRibs klGraphNonPartedNoCrossRibs
--     let startPartition = createStartPartition klGraphNonParted
--     let partition = itterateOverKerniganLine 10 startPartition
--     let bipart : GraphBiPartition OrderedGraphLine := 
--         if isGraphOptimalOrderWasFound 
--          then convertToBiPartitionWithOptimalOrder weightedGraph partition
--          else convertToBiPartitionWithTableOrder weightedGraph partition
    
--     let simplified = simplifyDependencies bipart
    
--     let functionsBodies : List TTImp := mapT generateFunctionBody simplified
    
--     let composedFunctionsErr = composeConcurrentFunctions (Builtin.snd res) functionName simplified (Builtin.fst res) functionsBodies
--     let stb : (List $ List Decl, List Name) := ([], [])
--     let composedFunctions = either (const stb) id composedFunctionsErr

--     let initErr = composeInitializationFunction functionName (Builtin.snd res) outputArgumentType (Builtin.snd composedFunctions) (Builtin.fst res)
--     let initStub : List Decl := []
--     let init = either (const initStub) id initErr

--     pure (Builtin.fst composedFunctions, init)
    -- traverse_ declare $ Builtin.fst composedFunctions
    -- let m = partWeight klGraphNonParted
    -- let m' = sum $ map weight klGraphNonParted.nodes
    -- let startPartition = createStartPartition klGraphNonParted
    -- pure init--$ show partition
    -- ?ret_rhs
    -- pure res
    -- traverse_ declare $ res
    -- declare $ snd res

-- testExecuteConcurrentObj : ?
-- testExecuteConcurrentObj = %runElab testExecuteConcurrentDebug (KerniganeLine 1 4) "concurrentFunction" Integer Integer $ \saw =>
--     let result1 = concurrentFunction1 << saw
--     in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
--     in let result3 = concurrentFunction3 << result2
--     in concurrentFunction4 << concat1 result1 result3 concatArguments

-- testExecuteConcurrentObj : ?
-- testExecuteConcurrentObj = 
-- %runElab makeFunctionConcurrent (KerniganeLine 10) "concurrentFunction" Integer Integer $ \saw =>
--     let result1 = concurrentFunction1 << saw
--     in let result2 = concurrentFunction2 << concat1 saw result1 concatArguments
--     in let result3 = concurrentFunction3 << result2
--     in concurrentFunction4 << concat1 result1 result3 concatArguments
------------------------------------------- Legacy Api ----------------------------------------------------------------------






















-------------------------------- Debug --------------------------------------------------------
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