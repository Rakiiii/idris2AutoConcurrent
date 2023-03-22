module Concurrent.Generator.FunctionGenerator

import Concurrent.Types.DataFlowGraph
import Concurrent.Types.Functions
import Concurrent.Utils.IR

import Data.List1
import Language.Reflection
import Language.Reflection.Types
import Language.Reflection.Pretty
import Language.Reflection.Syntax

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
MONOID_EFF_COMPOSITION = `((>>))
MONOID_COMPOSITION = `((>>=))
CHANNEL_PUT = `(channelPut)
CHANNEL_GET = `(channelGet)
UNDERSCORE = "_"
PLACEHOLDER = `(id 10)

concatParams : List String -> String
concatParams = joinBy UNDERSCORE

variableName : (paramName : String) -> (functionName : String) -> Int -> Name
variableName paramName functionName = MN (functionName ++ UNDERSCORE ++ paramName)
----------------------------------------------------------------------------------------------------------------------------------------------




















----------------------------------------------------------------------------------------------------------------------------------------------
mkConcurrentFunction : TTImp -> Elab (a -> b)
mkConcurrentFunction functionIR = check functionIR

constructVariableNameIfSavedFromFunction : SplittedFunctionBody -> Maybe Name
constructVariableNameIfSavedFromFunction function = 
    case function.resultVariable of
        ResultSaved _ names => firstOrDefault Nothing names
        ResultNotSaved      => Nothing

constructVariableNameIfSaved : DependentLine -> Maybe Name
constructVariableNameIfSaved = constructVariableNameIfSavedFromFunction . function

constructVariableNameOrEmpty : SplittedFunctionBody -> Name
constructVariableNameOrEmpty = maybe (UN $ Basic "") id . constructVariableNameIfSavedFromFunction

constructFunctionCall : DependentLine -> TTImp
constructFunctionCall dLine = 
    let (Parsed concatType arguments argumentNames) = dLine.function.function.argumentConstructor
    in let function = dLine.function.function.function
    in case concatType of 
        NoConstructor => foldl (.$) function arguments
        Pure => foldl (.$) function arguments
        -- тут не должно быть операторов конкатенации из DSL
        (Concatter _ compositionFunction name) => function .$ (foldl (.$) compositionFunction arguments)

generateChannelPut : Name -> TTImp -> TTImp
generateChannelPut channelName functionCall = CHANNEL_PUT .$ var channelName .$ functionCall

generateChannelPutFromVar : Name -> Name -> TTImp
generateChannelPutFromVar channelName = generateChannelPut channelName . var

isChannelGetNeeded : CleanDependentLine -> Bool
-- Тут надо проверить есть ли зависимости у этой функции и не сгенерировали ли вы случаем получение этой зависисмости дле предыдущей функции
isChannelGetNeeded = not . isNil . chanelGetDepenencies

-- Функция для генерации получения данных из канала и добавлять эту линиую пере линией функции
generateChannelGet : List SplittedFunctionBody -> TTImp -> TTImp
generateChannelGet [] scope = scope
-- тут должна быть композиция по всем deps
generateChannelGet (dep::deps) scope = generateChannelGet deps $ 
    MONOID_COMPOSITION .$ generateChannelGetInternal dep .$ (MkArg MW ExplicitArg (constructVariableNameIfSavedFromFunction dep) implicitFalse .=> scope) where
        generateChannelGetInternal : SplittedFunctionBody -> TTImp
        generateChannelGetInternal function = CHANNEL_GET .$ (var $ channelName function)

generateMonoidArrowComposition : TTImp -> TTImp -> TTImp
generateMonoidArrowComposition channelPutTTImp nextFunctionCall = MONOID_EFF_COMPOSITION .$ channelPutTTImp .$ nextFunctionCall

generateFunctionFromPartition : List CleanDependentLine -> FunctionLineWrapper
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


[OrdByOrder] Ord OrderedDependentLine where
     compare left right = compare left.order right.order

sort : Ord a => Table a -> Table a
sort = MkTable . sort . lines

-- Построить все внешние зависимости для каждой строки, причем только уникальные
cleanDependeciesBeetwenLines : List1 OrderedDependentLine -> CleanDependentLine
-- надо пробежаться и проверить зависимости, тк функция первая - то все зависимости на другие функции - внешние
cleanDependeciesBeetwenLines (firstLine:::[]) = MkCleanDependentLine firstLine.line firstLine.line.dependencies
-- Если линия не первая, то надо рассматривать последний в списке элемент, и проверять в списке до него совпадающие зависимости
cleanDependeciesBeetwenLines lines = 
    let currentLine = line $ last lines
    -- фильтруем все линии которые встречали в зависимостях до, тк получение значения из канала для них уже есть. 
    -- Последний элемент выкидываем из общего фильтра тк он текущий
    in MkCleanDependentLine currentLine $ filter ((flip predicat) lines) currentLine.dependencies where
        predicat : SplittedFunctionBody -> List1 OrderedDependentLine -> Bool
        predicat dependencie = not . contains dependencie . prevDependencies where
            prevDependencies : List1 OrderedDependentLine -> List SplittedFunctionBody
            prevDependencies = join . map (dependencies . line) . removeLast1

mapLaddered : (List1 a -> b) -> List a -> List b
mapLaddered f list = map f $ reverse $ foldl ladder [] list where
    ladder : List (List1 a) -> a -> List (List1 a)
    ladder [] a = [a:::[]]
    ladder (x::xs) a = (add a x)::x::xs--(cons a x)::(x::xs)


-- починить генерацию функции для конкатенации аргументов
-- починить добавление channelGet
public export
generateFunctionBody : Table OrderedDependentLine -> TTImp
generateFunctionBody table = 
    let sortedTable = sort table @{OrdByOrder}
    in unwrap $ generateFunctionFromPartition $ mapLaddered cleanDependeciesBeetwenLines sortedTable.lines


-- Надо сгенерировать декларацию и клаузу
-- Декларация должна содержать все каналы и стартовый аргумент, и все внутри монады IO
-- клауза должна содаржать каналы с правильным неймингом
-- Пример sIO в Main

public export
generateFunctionDeclaration : (dataDependenciesGraph : Table DependentLine) -> (functionName : Name) -> Decl
generateFunctionDeclaration table functionName = public' functionName $ (?createFunctionDeclaration_rhs table) .-> `(IO ())

public export
generateFunctionClause : (startArgumentType : TTImp) -> (dataDependenciesGraph : Table DependentLine) -> (function : TTImp) -> TTImp
generateFunctionClause startArgumentType graph function = let acc = function .$ startArgumentType in (foldl (.$) acc $ toChannels graph) where
    toChannels : Table DependentLine -> List TTImp

public export
generateFunctionDefenition : (functionName : Name) -> (clause : TTImp) -> (body : TTImp) -> Decl
generateFunctionDefenition functionName body clause = def functionName [clause .= body]

public export
generateFunction : (declaration : Decl) -> (defenition : Decl) -> List Decl
generateFunction declaration defenition = [declaration, defenition]

public export
composeFunctions : Namable a => (startArgumentType : TTImp) -> (baseFunctionName : String) -> (partition : a) -> (dataDependenciesGraph : Table DependentLine) -> (bodies : List TTImp) -> List $ List Decl
composeFunctions startArgumentType baseFunctionName namablePartition dataDependenciesGraph bodies = 
    let names = generateNames namablePartition baseFunctionName
    in let functions = map var names
    in let declarations = map (generateFunctionDeclaration dataDependenciesGraph) names
    in let clauses = map (generateFunctionClause startArgumentType dataDependenciesGraph) functions
    in let tuples = zip3 names clauses bodies 
    in let defenition = map (uncurryTuple generateFunctionDefenition) tuples
    in let description = zipWith generateFunction declarations defenition
    in ?composeFunction_rhs
----------------------------------------------------------------------------------------------------------------------------------------------