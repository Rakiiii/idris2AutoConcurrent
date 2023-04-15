module Concurrent.Generator.FunctionGenerator

import public Concurrent.Types.DataFlowGraph
import public Concurrent.Types.Functions
import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Utils.IR

import public Data.List1
import public Language.Reflection
import public Language.Reflection.Types
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax

%language ElabReflection


----------------------------------------------------------------------------------------------------------------------------------------------
-- tmp
public export
record FunctionLineWrapper where
    constructor WrapLetLine
    line : TTImp

unwrap : FunctionLineWrapper -> TTImp
unwrap = line
----------------------------------------------------------------------------------------------------------------------------------------------












----------------------------------------------------------------------------------------------------------------------------------------------
MONOID_EFF_COMPOSITION = `(Prelude.Interfaces.(>>))
MONOID_COMPOSITION     = `(Prelude.Interfaces.(>>=))


CHANNEL      = `(System.Concurrency.Channel)
CHANNEL_PUT  = `(System.Concurrency.channelPut)
CHANNEL_GET  = `(System.Concurrency.channelGet)
MAKE_CHANNEL = `(System.Concurrency.makeChannel)

FORK      = `(Prelude.IO.fork)
PRIM_IO   = `(PrimIO.IO)
THREAD_ID = `(PrimIO.ThreadID)


UNDERSCORE  = "_"
PLACEHOLDER = `(id 10)

concatParams : List String -> String
concatParams = joinBy UNDERSCORE

variableName : (paramName : String) -> (functionName : String) -> Int -> Name
variableName paramName functionName = MN (functionName ++ UNDERSCORE ++ paramName)

mapLaddered : (List1 a -> b) -> List a -> List b
mapLaddered f list = map f $ reverse $ foldl ladder [] list where
    ladder : List (List1 a) -> a -> List (List1 a)
    ladder []      a = [a:::[]]
    ladder (x::xs) a = (add a x)::x::xs--(cons a x)::(x::xs)

getFunctionType : TypedSplittedFunctionBody -> Elab $ Errorable TTImp
getFunctionType s =
    let maybeName = unVar s.function.function
    in case maybeName of
        Nothing   => do pure $ errorExtra s.function.function "Cannot get name for function"
        Just name => do names <- getType name
                        case names of
                                []              => pure $ errorExtra s.function.function "Cannot get type for function"
                                ((_, type)::xs) => pure $ Right type

explicitArgNamed : Name -> TTImp -> Arg
explicitArgNamed name type = MkArg MW ExplicitArg (Just name) type

explicitArg : TTImp -> Arg
explicitArg = MkArg MW ExplicitArg Nothing

explicitArgChannel : TTImp -> Arg
explicitArgChannel type = MkArg MW ExplicitArg Nothing $ CHANNEL .$ type
----------------------------------------------------------------------------------------------------------------------------------------------




















----------------------------------------------------------------------------------------------------------------------------------------------
mkConcurrentFunction : TTImp -> Elab (a -> b)
mkConcurrentFunction functionIR = check functionIR

generateMonoidArrowComposition : TTImp -> TTImp -> TTImp
generateMonoidArrowComposition channelPutTTImp nextFunctionCall = MONOID_EFF_COMPOSITION .$ channelPutTTImp .$ nextFunctionCall

constructVariableNameIfSavedFromFunction : TypedSplittedFunctionBody -> Maybe Name
constructVariableNameIfSavedFromFunction function = 
    case function.resultVariable of
        ResultSaved _ names => firstOrDefault Nothing names
        ResultNotSaved      => Nothing

constructVariableNameIfSaved : DependentLine TypedSplittedFunctionBody -> Maybe Name
constructVariableNameIfSaved = constructVariableNameIfSavedFromFunction . function

constructVariableNameOrEmpty : TypedSplittedFunctionBody -> Name
constructVariableNameOrEmpty = maybe (UN $ Basic "") id . constructVariableNameIfSavedFromFunction

constructFunctionCall : DependentLine TypedSplittedFunctionBody -> TTImp
constructFunctionCall dLine = 
    let (Parsed concatType arguments argumentNames) = dLine.function.function.argumentConstructor
    in let function = dLine.function.function.function
    in case concatType of 
        NoConstructor                          => foldl (.$) function arguments
        Pure                                   => foldl (.$) function arguments
        -- тут не должно быть операторов конкатенации из DSL
        (Concatter _ compositionFunction name) => function .$ (foldl (.$) compositionFunction arguments)

generateChannelPut : Name -> TTImp -> TTImp
generateChannelPut channelName variable = CHANNEL_PUT .$ var channelName .$ variable

generateChannelPutFromVar : Name -> Name -> TTImp
generateChannelPutFromVar channelName = generateChannelPut channelName . var

isChannelGetNeeded : CleanDependentLine TypedSplittedFunctionBody -> Bool
-- Тут надо проверить есть ли зависимости у этой функции и не сгенерировали ли вы случаем получение этой зависисмости дле предыдущей функции
isChannelGetNeeded = not . isNil . chanelGetDepenencies

-- Функция для генерации получения данных из канала и добавлять эту линиую пере линией функции
generateChannelGet : List TypedSplittedFunctionBody -> TTImp -> TTImp
generateChannelGet [] scope = scope
-- тут должна быть композиция по всем deps
-- Тут должен быть тип
generateChannelGet (dep::deps) scope = generateChannelGet deps $ 
    MONOID_COMPOSITION .$ generateChannelGetInternal dep .$ (MkArg MW ExplicitArg (constructVariableNameIfSavedFromFunction dep) implicitFalse .=> scope) where
        generateChannelGetInternal : TypedSplittedFunctionBody -> TTImp
        generateChannelGetInternal function = CHANNEL_GET .$ (var $ channelName function)

generateFunctionFromPartition : List (CleanDependentLine TypedSplittedFunctionBody) -> FunctionLineWrapper
-- Что вернуть когда пустой массив? ибо не возможно
generateFunctionFromPartition [] = WrapLetLine PLACEHOLDER
generateFunctionFromPartition [line] =
            WrapLetLine $ 
                generateChannelGet line.chanelGetDepenencies $ 
                    generateChannelPut (channelName line) $ constructFunctionCall line.line
generateFunctionFromPartition (line::lines) = 
            -- Тут должен быть вызов для channelGet уже вычисленных результатов
            let function = constructFunctionCall line.line
            in WrapLetLine $ 
                    -- ошибка, тут не (line::lines), тут должен быть оригинлас списка.
                    -- Может лучше заранее пробежаться и сформировать список необходимых для генерации зависимоестей
                    -- СleanDependentLine как раз такой
                generateChannelGet line.chanelGetDepenencies $ 
                    case constructVariableNameIfSaved line.line of
                        -- Случай когда это последний вызов функции который возвращает результат
                        Nothing           => generateChannelPut (channelName line) function
                        Just variableName => iLet MW variableName implicitTrue function $ 
                                                -- тут можно не всегда генерировать сhannelPut
                                                generateMonoidArrowComposition (generateChannelPutFromVar (channelName line) variableName) $
                                                    unwrap $ generateFunctionFromPartition lines


[OrdByOrder] Eq a => Ord (OrderedDependentLine a) where
     compare left right = compare left.order right.order

sort : Ord a => Table a -> Table a
sort = MkTable . sort . lines

-- Построить все внешние зависимости для каждой строки, причем только уникальные
cleanDependeciesBeetwenLines : List1 (OrderedDependentLine TypedSplittedFunctionBody) -> CleanDependentLine TypedSplittedFunctionBody
-- надо пробежаться и проверить зависимости, тк функция первая - то все зависимости на другие функции - внешние
cleanDependeciesBeetwenLines (firstLine:::[]) = MkCleanDependentLine firstLine.line firstLine.line.dependencies
-- Если линия не первая, то надо рассматривать последний в списке элемент, и проверять в списке до него совпадающие зависимости
cleanDependeciesBeetwenLines lines = 
    let currentLine = line $ last lines
    -- фильтруем все линии которые встречали в зависимостях до, тк получение значения из канала для них уже есть. 
    -- Последний элемент выкидываем из общего фильтра тк он текущий
    in MkCleanDependentLine currentLine $ filter ((flip predicat) lines) currentLine.dependencies where
        predicat : TypedSplittedFunctionBody -> List1 (OrderedDependentLine TypedSplittedFunctionBody) -> Bool
        predicat dependencie = not . contains dependencie . prevDependencies where
            prevDependencies : List1 (OrderedDependentLine TypedSplittedFunctionBody) -> List TypedSplittedFunctionBody
            prevDependencies = join . map (dependencies . line) . removeLast1


-- починить генерацию функции для конкатенации аргументов
-- починить добавление channelGet
public export
generateFunctionBody : Table (OrderedDependentLine TypedSplittedFunctionBody) -> TTImp
generateFunctionBody table = 
    let sortedTable = sort table @{OrdByOrder}
    in unwrap $ generateFunctionFromPartition $ mapLaddered cleanDependeciesBeetwenLines sortedTable.lines


-- Надо сгенерировать декларацию и клаузу
-- Декларация должна содержать все каналы и стартовый аргумент, и все внутри монады IO
-- клауза должна содаржать каналы с правильным неймингом
-- Пример sIO в Main

createFunctionDeclarationArguments :  (startArgumentType : TTImp) -> Table (DependentLine TypedSplittedFunctionBody) -> Errorable TTImp
createFunctionDeclarationArguments startArgumentType (MkTable lines) =
    createFunctionDeclarationArgumentsInternal startArgumentType $ map function lines where
   
    createFunctionDeclarationArgumentsInternal : (startArgumentType : TTImp) -> List TypedSplittedFunctionBody -> Errorable TTImp
    createFunctionDeclarationArgumentsInternal startArgumentType [] = error "Cant construct type for function declaration"
    createFunctionDeclarationArgumentsInternal startArgumentType (lastArgument::[]) = 
        Right $ (explicitArgChannel lastArgument.returnType.type) .-> explicitArg startArgumentType .-> `(IO ()) 

    createFunctionDeclarationArgumentsInternal startArgumentType (argument::arguments) =
        (createFunctionDeclarationArgumentsInternal startArgumentType arguments
            `rxMap` (.->) (explicitArgChannel argument.returnType.type))
        
public export
generateFunctionDeclaration :  (startArgumentType : TTImp) -> (dataDependenciesGraph : Table $ DependentLine TypedSplittedFunctionBody) -> (functionName : Name) -> Errorable Decl
generateFunctionDeclaration startArgumentType table functionName = 
    (createFunctionDeclarationArguments startArgumentType table
        `rxMap` public' functionName)

public export
generateFunctionClause : (startArgumentName : String) -> (dataDependenciesGraph : Table $ DependentLine TypedSplittedFunctionBody) -> (function : TTImp) -> TTImp
generateFunctionClause startArgumentName graph function = (foldl (.$) function $ toChannels graph) .$ (bindVar startArgumentName) where
    toChannels : Table (DependentLine TypedSplittedFunctionBody) -> List TTImp
    toChannels (MkTable lines) = map (\dLine => bindVar $ channelNameStr dLine.function) lines

-- bindVar

public export
generateFunctionDefenition : (functionName : Name) -> (clause : TTImp) -> (body : TTImp) -> Decl
generateFunctionDefenition functionName clause body = def functionName [clause .= body]

public export
generateFunction : (declaration : Decl) -> (defenition : Decl) -> List Decl
generateFunction declaration defenition = [declaration, defenition]

public export
composeConcurrentFunctions : Namable a => (startArgument : InputArgument) -> (baseFunctionName : String) -> (partition : a) -> (dataDependenciesGraph : Table $ DependentLine TypedSplittedFunctionBody) -> (bodies : List TTImp) -> Errorable (List $ List Decl, List Name)
composeConcurrentFunctions startArgument baseFunctionName namablePartition dataDependenciesGraph bodies = 
    let names = generateNames namablePartition baseFunctionName
    in let functions = map var names
    in let declarations = map (generateFunctionDeclaration startArgument.type dataDependenciesGraph) names
    in let clauses = map (generateFunctionClause startArgument.name dataDependenciesGraph) functions
    in let tuples = zip3 names clauses bodies 
    in let defenition = map (uncurryTuple generateFunctionDefenition) tuples
    in declarations
          `rxJoinListEither` ()
          `rxMap` (\decl => zipWith generateFunction decl defenition)
          `rxMap` (\description => (description, names))
----------------------------------------------------------------------------------------------------------------------------------------------











-------------------------------------------------- MAKE CHANNEL ----------------------------------------------------------------------------

generateMakeChannel : (channelType : TTImp) -> TTImp
generateMakeChannel channelType = MAKE_CHANNEL .! ("a", channelType) .! ("io", var "PrimIO.IO")

constructMakeChannelLine : DependentLine TypedSplittedFunctionBody -> (scope : TTImp)-> TTImp
constructMakeChannelLine line scope = genMakeChannel line.function.returnType.type where

    makeChannelComposition : TTImp -> TTImp
    makeChannelComposition makeChannel = MONOID_COMPOSITION .$ makeChannel .$ 
                                            (MkArg MW ExplicitArg (Just $ channelName line.function) implicitFalse .=> scope)
                                                            
    genMakeChannel : TTImp -> TTImp
    genMakeChannel errorable = makeChannelComposition $ generateMakeChannel errorable


generateChannelInitFromPartition : List (DependentLine TypedSplittedFunctionBody) -> (forkFunctions : TTImp) -> Errorable TTImp
generateChannelInitFromPartition [] _                         = error "Try to generate channeles init for empty function"
generateChannelInitFromPartition (lastLine::[]) forkFunctions = Right $ constructMakeChannelLine lastLine forkFunctions
generateChannelInitFromPartition (line::lines) forkFunctions  = generateChannelInitFromPartition lines forkFunctions
                                                                    `rxMap` constructMakeChannelLine line
----------------------------------------------------------------------------------------------------------------------------------------------







------------------------------------------------------------ FORK ----------------------------------------------------------------------------
channaleList : List (DependentLine TypedSplittedFunctionBody) -> List TTImp
channaleList list = map (var . channelName) $ map function list

constructForkFunctionLine : (dataDependenciesGraph : List $ DependentLine TypedSplittedFunctionBody) -> (functionName : Name) -> (inputArgument : Name) -> (scope : TTImp) -> TTImp
constructForkFunctionLine dataDependenciesGraph functionName inputArgumentName scope = makeForkComposition $ makeFork dataDependenciesGraph functionName where--?constructForkFunctionLine_rhs 

        makeForkComposition : TTImp -> TTImp
        -- Нужны ли типы в MONOID_COMPOSITION ??? 
        makeForkComposition fork = MONOID_COMPOSITION .$ fork .$
            (MkArg MW ExplicitArg Nothing THREAD_ID .=> scope)

        makeFork : List (DependentLine TypedSplittedFunctionBody) -> (functionName : Name) -> TTImp
        makeFork list functionName = 
            let channelList = channaleList list
            in let functionCall = foldl (.$) (var functionName) channelList
            in FORK .$ (functionCall .$ var inputArgumentName)

generateFunctionForkCall : (concurrentFunctionsNames : List Name) -> (dataDependenciesGraph : List $ DependentLine TypedSplittedFunctionBody) -> (inputArgumentName : Name) -> (lastChannelGet : TTImp) -> Errorable TTImp
generateFunctionForkCall []             dataDependenciesGraph inputArgumentName lastChannelGet = error "Try to generate function forks without functions"
generateFunctionForkCall (lastName::[]) dataDependenciesGraph inputArgumentName lastChannelGet = Right $ constructForkFunctionLine dataDependenciesGraph lastName inputArgumentName lastChannelGet
generateFunctionForkCall (name::names)  dataDependenciesGraph inputArgumentName lastChannelGet = 
    generateFunctionForkCall names dataDependenciesGraph inputArgumentName lastChannelGet
        `rxMap` constructForkFunctionLine dataDependenciesGraph name inputArgumentName








----------------------------------------------------------------------------------------------------------------------------------------------
generateLastChannelGet : (concurrentFunctionsNames : List Name) -> (dataDependenciesGraph : List $ DependentLine TypedSplittedFunctionBody) -> Errorable TTImp
generateLastChannelGet concurrentFunctionsNames [] = error "Cannot generate channelGet for last channel"
generateLastChannelGet concurrentFunctionsNames (line::lines) = 
    let lastFunction = lastOrDefault line lines
    in let lastChannel = var $ channelName lastFunction
    in Right $ (CHANNEL_GET .! ("a", lastFunction.function.returnType.type) .! ("io", PRIM_IO) ) .$ lastChannel
----------------------------------------------------------------------------------------------------------------------------------------------





----------------------------------------------------------------------------------------------------------------------------------------------
-- Функция котороя генерирует иницализационную функцию, которая создает частичную функцию с передачей каналов
-- Пример функции initFunctionExample
-- надо добавить передачу в функцию аргумента
public export
generateInitializationFunctionBody : (concurrentFunctionsNames : List Name) -> (graph : Table $ DependentLine TypedSplittedFunctionBody) -> (inputArgument : InputArgument) -> Errorable TTImp--Elab $ a -> IO b
generateInitializationFunctionBody concurrentFunctionsNames table inputArgument = do
    -- let sortedTable = sort table @{OrdByOrder}
    -- let cleanedLines = mapLaddered cleanDependeciesBeetwenLines sortedTable.lines
    generateLastChannelGet concurrentFunctionsNames table.lines
        `rxMap` generateFunctionForkCall concurrentFunctionsNames table.lines (name inputArgument.name)
        `rxJoin` () 
        `rxMap` generateChannelInitFromPartition table.lines
        `rxJoin` ()

public export
generateInitializationFunctionClause : (functionName : String) -> InputArgument -> TTImp
generateInitializationFunctionClause functionName inputArgument = (var $ name functionName) .$ bindVar inputArgument.name

public export
generateInitializationFunctionDeclaration : (functionName : String) -> InputArgument -> (outputArgumentType : TTImp) -> Decl
generateInitializationFunctionDeclaration functionName inputArgument outputArgumentType = 
    public' (name functionName) $ explicitArg inputArgument.type .-> `(IO ~(outputArgumentType))



public export
composeInitializationFunction : (functionName : String) -> (inputArgument : InputArgument) -> (outputArgumentType : TTImp) -> (concurrentFunctionsNames : List Name) -> (graph : Table $ DependentLine TypedSplittedFunctionBody) -> ErrorableList Decl
composeInitializationFunction functionName inputArgument outputArgumentType concurrentFunctionsNames graph = do
    let name = name functionName
    let clause = generateInitializationFunctionClause functionName inputArgument
    let declaration = generateInitializationFunctionDeclaration functionName inputArgument outputArgumentType
    generateInitializationFunctionBody concurrentFunctionsNames graph inputArgument
        `rxMap` generateFunctionDefenition name clause
        `rxMap` generateFunction declaration
