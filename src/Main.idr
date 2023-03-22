module Main

import Concurrent.Api.ConcurrentDsl
import Concurrent.Types.Functions
import Concurrent.Types.DataFlowGraph
import Concurrent.Utils.IR
import Concurrent.Parser.Splitter
import Concurrent.Parser.GraphConstructor
import Concurrent.Partition.GraphPartitioner
import Concurrent.Generator.FunctionGenerator

import Language.Reflection
import Language.Reflection.Types
import Language.Reflection.Pretty
import Language.Reflection.Syntax
import Text.PrettyPrint.Bernardy

import System.Concurrency



%default total
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

elabGenerator: a -> (ConcurrentWrap a -> ConcurrentWrap b) -> b
elabGenerator = ?elabGenerator_impl

startArgument: Integer
startArgument = 1

testFunctionMonoidBody: ConcurrentWrap Integer -> ConcurrentWrap Integer
testFunctionMonoidBody startArgumentWrapped = do 
    result1 <- concurrentFunction1 << startArgumentWrapped 
    result2 <- concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
    result3 <- concurrentFunction3 << pure result1
    result4 <- concurrentFunction4 << concatPure result1 result2 concatArguments
    result5 <- concurrentFunction5 << pure result4
    result6 <- concurrentFunction6 << pure result4
    concurrentFunction7 << concatPure result5 result6 concatArguments
    
testFunctionMonoidBodyTTImp: ?
testFunctionMonoidBodyTTImp = `(do 
    result1 <- concurrentFunction1 << startArgumentWrapped 
    result2 <- concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
    result3 <- concurrentFunction3 << pure result1
    result4 <- concurrentFunction4 << concatPure result1 result2 concatArguments
    result5 <- concurrentFunction5 << pure result4
    result6 <- concurrentFunction6 << pure result4
    concurrentFunction7 << concat2 result5 result6 concatArguments
)

testFunctionMonoidBodyTTImpSmall: ?
testFunctionMonoidBodyTTImpSmall = `(do 
    result1 <- concurrentFunction1 << startArgumentWrapped 
    result2 <- concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
    result3 <- concurrentFunction3 << pure result1
    concurrentFunction4 << concatPure result1 result2 concatArguments
)

tst : ?
tst = snd $ unApp testFunctionMonoidBodyTTImp

testFunction: ? --Integer -> (ConcurrentWrap Integer -> ConcurrentWrap Integer) -> Integer
testFunction = elabGenerator startArgument $ \startArgumentWrapped => do
result1 <- concurrentFunction1 << startArgumentWrapped 
result2 <- concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
return result2 

testFunctionBody: ?
testFunctionBody = `(elabGenerator startArgument $ \startArgumentWrapped => do
result1 <- concurrentFunction1 << startArgumentWrapped 
result2 <- concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
pure result2)

lambdaBody: ?
lambdaBody = `(\startArgumentWrapped => do
result1 <- concurrentFunction1 << startArgumentWrapped 
result2 <- concurrentFunction2 << concat1 startArgumentWrapped result1 concatArguments
pure result2)

lambdaBodyLet: ?
lambdaBodyLet = `(\startArgumentWrapped => do
let result1 = concurrentFunction1 << startArgumentWrapped 
let result2 = concurrentFunction2 << concat2 startArgumentWrapped result1 concatArguments
result2)

------------------------------------TTImp to Graph parser-------------------------------------------------------------------------------------------










----------------------------------------------------------------------------------------------------------------------------------------------------
getFunctionBodyAndArguments : (ConcurrentWrap a -> ConcurrentWrap b) -> (Maybe Arg, TTImp)
getFunctionBodyAndArguments function = ?getFunctionBodyAndArguments_impl

covering
createConcurrentExecution: (ConcurrentWrap a -> ConcurrentWrap b) -> Elab ()
createConcurrentExecution function = do
    baseFunctionName <- genSym "concurrentFunctionBaseName"
    let (maybeArg, body) = getFunctionBodyAndArguments function
    let functions = splitFunctionByMonoidArrow body
    let dataDependencieGraph = constructDataDependencieGraph functions
    let partition = simplifyDependencies $ doBiPartition RandomBiPartitioner WeightAll1 dataDependencieGraph
    let simplifyedPartition = simplifyDependencies partition
    let functionsBodies : List TTImp :=  mapT generateFunctionBody simplifyedPartition
    let functions = composeFunctions ?argType ?baseFunctionName partition dataDependencieGraph functionsBodies
    foldl (\_, decl => do declare decl) (pure ()) functions
    -- putDeclaration functions where
    --     putDeclaration : List (List Decl) -> Elab ()
    --     putDeclaration [] = pure ()
    --     putDeclaration (decl::decls) = 
    --         do 
    --             declare decl
    --             putDeclaration decls
----------------------------------------------------------------------------------------------------------------------------------------------------






















-------------------------------- Debug --------------------------------------------------------
placeholder : a -> (b -> a)
placeholder x = \_ => x

covering
splitFunctionByMonoidArrowInternalDebug : List SplittedFunctionBody -> TTImp -> (List SplittedFunctionBody, Maybe TTImp)
splitFunctionByMonoidArrowInternalDebug accumulator bodyPart = 
    let (arguments, body) = unLambda bodyPart 
    -- Сначала надо проверить на наличие моноидальной стрелки - если есть то парсим как обычно если нет то парсим как аргементы сразу
    in let (monoidArrow, concurrentFunctionCall) = unApp body 
    in let dependencies = ResultNotSaved
    in let maybeConcurrentFunctionAndTail = parseArgumentsList concurrentFunctionCall 
    in case maybeConcurrentFunctionAndTail of
        Just (concurrentFunctionIR, maybeTail) => case maybeTail of
                                                Just tail => ((accumulator ++ [MkSplittedFunctionBody dependencies concurrentFunctionIR]), Just tail)
                                                Nothing   => (accumulator ++ [MkSplittedFunctionBody dependencies concurrentFunctionIR], Nothing)
        Nothing                           => (accumulator, Nothing)
 
splitFunctionByMonoidArrowInternalDebugFromPair : (List SplittedFunctionBody, Maybe TTImp) -> (List SplittedFunctionBody, Maybe TTImp)
splitFunctionByMonoidArrowInternalDebugFromPair (list, Nothing) = (list, Nothing)
splitFunctionByMonoidArrowInternalDebugFromPair (list, Just tail) = splitFunctionByMonoidArrowInternalDebug list tail

covering
s' : ?
s' = constructDataDependencieGraph $ splitFunctionByMonoidArrow testFunctionMonoidBodyTTImp

s: ?
s = -- splitFunctionByMonoidArrowInternalDebugFromPair $
        splitFunctionByMonoidArrowInternalDebugFromPair $
            splitFunctionByMonoidArrowInternalDebugFromPair $ 
                splitFunctionByMonoidArrowInternalDebugFromPair $ 
                    splitFunctionByMonoidArrowInternalDebugFromPair $
                        splitFunctionByMonoidArrowInternalDebugFromPair $
                            splitFunctionByMonoidArrowInternalDebug [] testFunctionMonoidBodyTTImp

s1Syntax : Integer -> Integer
s1Syntax param1 = 
    let variable = concurrentFunction1 param1 
    in let variable2 = concurrentFunction2 variable
    in concurrentFunction3 variable2

s1 : ?
s1 = `(let variable = concurrentFunction1 param1 
    in let variable2 = concurrentFunction2 variable
    in concurrentFunction3 variable2
)

covering
s2' : ?
s2' = simplifyDependencies $ doBiPartition RandomBiPartitioner WeightAll1 $ constructDataDependencieGraph $ splitFunctionByMonoidArrow testFunctionMonoidBodyTTImpSmall

covering
s2 : List TTImp
s2 = mapT generateFunctionBody s2'--mapLaddered cleanDependeciesBeetwenLines s2'''


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

sIO3' : ?
sIO3' = `(do
    let variable = concurrentFunction1 param1 
    channelPut depChannel1 variable
    let variable2 = concurrentFunction2 variable
    variable3 <- channelGet depChannel2
    variable4 <- channelGet depChannel3
    channelPut resultChannel $ concurrentFunction3 $ concatArguments variable2 variable3)

generatorTestFunction : ConcurrentWrap Integer -> ConcurrentWrap Integer
generatorTestFunction saw = do 
    result1 <- (concurrentFunction1 << saw)
    result2 <- (concurrentFunction2 << concat1 saw result1 concatArguments)
    result3 <- (concurrentFunction3 << pure result1)
    concurrentFunction4 << concatPure result1 result2 concatArguments

toTTImp : (ConcurrentWrap a -> ConcurrentWrap a) -> Elab TTImp
toTTImp f = do Reflection.quote f

sIO4 : ?
sIO4 = do  logTerm "concurrentWrap" 0 "generatorTestFunction" $ !(toTTImp generatorTestFunction)
-- $ \saw => do 
--     result1 <- (concurrentFunction1 << saw)
--     result2 <- (concurrentFunction2 << concat1 saw result1 concatArguments)
--     result3 <- (concurrentFunction3 << pure result1)
--     concurrentFunction4 << concatPure result1 result2 concatArguments