module Concurrent.Parser.Splitter



import public Concurrent.Types.Functions
import public Concurrent.Utils.IR

import public Data.Maybe
import public Data.List
import public Language.Reflection
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Text.PrettyPrint.Bernardy

%default total
%language ElabReflection

------------------------------------------- Legacy Api ----------------------------------------------------------------------


-- Принимает вызов паралельной функции и парсит его во внутрнее представление
-- Первое значение в списке апликаций - функция, вторая аргументы функции
-- Это должно парсить параметры не через unApp тк апликация там если и есть то внутри
-- tmp public export
getType : (probablyConcatter : TTImp) -> List TTImp -> List String -> (ArgumentConstructorType, (List TTImp, List String))
getType trivialArgument         []         args      = (NoConstructor, [trivialArgument], args)
getType _         [argument] args      = (Pure, [argument], args)
getType concatter arguments  args      = (Concatter concatter (lastOrDefault concatter arguments) $ lastOrEmpty args, removeLast arguments, removeLast args)

parseConcurrentFunctionArguments : TTImp -> ArgumentConstructor
parseConcurrentFunctionArguments call = 
    let (concatter, arguments) = unApp call
    in uncurryTuple Parsed $
            getType concatter arguments $ 
                map removeMaybeFromString $ 
                    filter isJust $ 
                        map extractMaybeNameString  $ 
                            analyzeCallArguments concatter arguments where
                                analyzeCallArguments : TTImp -> List TTImp -> List TTImp
                                analyzeCallArguments trivialArgument []         = [trivialArgument]
                                analyzeCallArguments pureCall        [argument] = [argument]
                                analyzeCallArguments concatFunction  arguments  = arguments


createConcurrentFunction : TTImp -> Maybe ConcurrentFunction
createConcurrentFunction operatorCall = 
let (operator, callArguments) = unApp operatorCall in 
    parseCallArguments callArguments where
    parseCallArguments : List TTImp -> Maybe ConcurrentFunction
    parseCallArguments [x1, x2] = Just (MkConcurrentFunction x1 $ parseConcurrentFunctionArguments x2)
    parseCallArguments _ = Nothing  

-- Если в списке 2 значения - то первый в списке есть вызов паралельной функции, а второй остальная функция
-- это не верно, весь список это аргументы
-- tmp
public export
parseArgumentsList : List TTImp -> Maybe (ConcurrentFunction, Maybe TTImp)
parseArgumentsList [x1] =  (\m => (m, Nothing)) <$> createConcurrentFunction x1
parseArgumentsList [x1, x2] =  (\m => (m, Just x2)) <$> createConcurrentFunction x1
parseArgumentsList _ = Nothing
    

parseResultVariable : Arg -> (String, Maybe Name)
parseResultVariable (MkArg _ _ maybeName _ ) = (stringOrEmpty $ join $ nameToString <$> maybeName, maybeName)

parseVariableForSave : TTImp -> ResultVariable
parseVariableForSave = (uncurry ResultSaved) . unzip . (map parseResultVariable) . fst . unLambda

-- Рекурсивно разбивает тело функции на набор паралельных вызовов
-- Сначала отрезает часть с внешним аргументов (лямбда-шаблон)
-- Затем убирает апликацию монадической стрелки и пвтается распарсить первый аргумент
covering
splitFunctionByMonoidArrowInternal : List SplittedFunctionBody -> TTImp -> List SplittedFunctionBody
-- Сначала надо проверить на наличие моноидальной стрелки - если есть то парсим как обычно если нет то парсим как аргементы сразу
splitFunctionByMonoidArrowInternal accumulator bodyPart =
    let application = snd $ unLambda $ bodyPart in
    if isMonoidArrowApplication application
        then
            let (arguments, body) = unLambda bodyPart
            in let (monoidArrow, concurrentFunctionCall) = unApp body 
            in let maybeConcurrentFunctionAndTail = parseArgumentsList concurrentFunctionCall 
            -- in valueOrDefault accumulator $ 
            --         map (\(concurrentFunctionIR, maybeTail) => valueOrDefault (accumulator ++ [MkSplittedFunctionBody ResultNotSaved concurrentFunctionIR]) $ 
            --                 map (\tail => let functionResult = parseVariableForSave tail in splitFunctionByMonoidArrowInternal (accumulator ++ [MkSplittedFunctionBody functionResult concurrentFunctionIR]) tail) maybeTail
            --             ) maybeConcurrentFunctionAndTail 
            in case maybeConcurrentFunctionAndTail of
                Nothing                                => accumulator
                Just (concurrentFunctionIR, maybeTail) =>       
                                                          case maybeTail of
                                                                    Nothing   => accumulator ++ [MkSplittedFunctionBody ResultNotSaved concurrentFunctionIR]
                                                                    Just tail => 
                                                                                let functionResult = parseVariableForSave tail 
                                                                                in splitFunctionByMonoidArrowInternal (accumulator ++ [MkSplittedFunctionBody functionResult concurrentFunctionIR]) tail
        else 
            let (arguments, body) = unLambda bodyPart 
            in let functionResult = ResultNotSaved
            in let maybeConcurrentFunction = createConcurrentFunction body
            in case maybeConcurrentFunction of
                Nothing                   => accumulator 
                Just concurrentFunctionIR => accumulator ++ [MkSplittedFunctionBody functionResult concurrentFunctionIR]

public export
covering
splitFunctionByMonoidArrow: TTImp -> List SplittedFunctionBody
splitFunctionByMonoidArrow = splitFunctionByMonoidArrowInternal [] 

