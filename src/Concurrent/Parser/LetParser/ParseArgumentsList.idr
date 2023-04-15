module Concurrent.Parser.LetParser.ParseArgumentsList


import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Parser.LetParser.Utils
import public Concurrent.Utils.IR


import public Data.Maybe
import public Language.Reflection
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax

%default total
%language ElabReflection

-- Принимает вызов паралельной функции и парсит его во внутрнее представление
-- Первое значение в списке апликаций - функция, вторая аргументы функции
-- Это должно парсить параметры не через unApp тк апликация там если и есть то внутри
-- tmp public export
getType : (probablyConcatter : TTImp) -> List TTImp -> List String -> (ArgumentConstructorType, (List TTImp, List String))
getType trivialArgument         []         args      = (NoConstructor, [trivialArgument], args)
getType _         [argument] args      = (Pure, [argument], args)
getType concatter arguments  args      = (Concatter concatter (lastOrDefault concatter arguments) $ lastOrEmpty args, removeLast arguments, removeLast args)

--tmp public export
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

isCallOperatorCall : TTImp -> Bool
isCallOperatorCall func = (extractCallOperator $ fst $ unApp func) == CallOperator


-- тут надо понять какие могут быть типы вызовов функции и рассмотреть все варианты
-- на первый взгляд 4 варианта
--          В этих двух случаях надо сделать unApp - в первом аргументе будут (someConcatter или <<) в списке может быть набор разных комбинаций из аргументов
--          надо проверить каждый элемент из списка и поместить все это дело в регистр
--                  concFunction << someConcatter (otherConcFunction << ...) ...
--                  concFunction << someConcatter args ...
--                  concFunction << (otherConcFunction << ...) ...
--                  concFunction << arg
--          Как проверить что аргумент - вызов???
--          надо сделать unApp - если список не пуст - вызов
-- function - вызываемая функция; functionArgument аргументы для этой функции - их и надо проверять
--tmp public export
createConcurrentFunction : CallRegister -> (function : TTImp) -> (functionArgument : TTImp) -> Errorable (ConcurrentFunction, List TTImp, CallRegister)
createConcurrentFunction callRegister function functionArgument = 
    -- кейс 1-3
    case functionArgument of 
        IApp _ _ _ => if isCallOperatorCall functionArgument
                        -- кейс 3
                        -- если аргумент - паралельный вызов - делаем следующее
                        -- проверяем регистр на наличие вызова ЦЕЛИКОМ
                        -- если вызов есть в регистре - заменяем на результат вызова
                        -- если вызова нет в регистре - надо пропарсить со старта этот вызов
                        -- надо вернуть ключ и как-то обработать
                        then 
                            -- Проверяем встречали ли мы такой аргумент для вызова функции
                            let maybeKey = inRegister callRegister functionArgument 
                                in case maybeKey of 
                                    -- если встречали - тогда заменяем вызов функции на переменную в которую это будет сохранено
                                    Just key => 
                                                let variableName = generateResultVariableForCall key
                                                in map (invTuple [] callRegister) $ 
                                                        map (concurrentFunctionSimple function) variableName
                                    -- тут должны обновить регистр и вернуть переменные для парсинга
                                    Nothing  => 
                                                -- добавление в регистр тут != парсинг --> Решение добавить флаг для который показывает что функция не пропаршена
                                                let (newRegister, key) = updateRegister callRegister functionArgument False
                                                in let variableName = generateResultVariableForCall key
                                                in map (invTuple [functionArgument] newRegister) $ 
                                                        map (concurrentFunctionSimple function) variableName
                        -- кейс 1,2
                        -- тут надо проверить полученные аргументы
                        -- если аргумент паралельный вызов - логика из кейс 3 + заменить на название переменной из регистра
                        -- тут не пустой список возвращаем, его надо заполнить исходя из новых вложенных вызовов
                        else map (mapFst (MkConcurrentFunction function)) $ parseConcatterArguments callRegister functionArgument
        -- кейс 4
        _          => Right (concurrentFunctionSimple function functionArgument, [], callRegister) where

            -- обертка для создания паралельной функции
            concurrentFunctionSimple : TTImp -> TTImp -> ConcurrentFunction
            concurrentFunctionSimple func = MkConcurrentFunction func . parseConcurrentFunctionArguments

            -- в этой функции должны пробежаться по всем аргументам и понять что с ними делать
            -- Возможны 2 случая:
            --          Если уже встречали такой аргумент - просто меняем на переменную
            --          Если не встречали -> меняем на переменную и возвращаем наверх для парсинга новой конструкции + добавляем в регистр
            mapArgs : ((CallRegister, List TTImp), TTImp) -> ((CallRegister, List TTImp), TTImp)
            mapArgs ((callRegister, forParse), arg) = 
                if isCallOperatorCall arg 
                    then 
                        let maybeKey = inRegister callRegister arg 
                        in case maybeKey of
                                -- TODO :: подумать надо ли safety тут
                                Just key => ((callRegister, forParse), generateResultVariableForCallUnsafe key)
                                Nothing  => 
                                    let (newRegister, key) = updateRegister callRegister arg False
                                    in ((newRegister, arg::forParse), generateResultVariableForCallUnsafe key)
                                                                        
                    else ((callRegister, forParse), arg)

            -- надо чтобы скинуть (.!) с вызовов
            removeNamedApp : TTImp -> TTImp
            removeNamedApp x                    = case x of
                           INamedApp _ newX _ _ => removeNamedApp newX
                           _                    => x

            -- Cначала надо распарсить аргументы конкаттера
            -- затем надо провебежаться по всем кроме последнего
            -- последний аргумент - функция конкатенации
            parseConcatterArguments : CallRegister -> TTImp -> Errorable (ArgumentConstructor, List TTImp, CallRegister)
            parseConcatterArguments callRegister argument = 
                let (concatter, concatterArgs) = unApp argument 
                in let pureConcatter = removeNamedApp concatter
                in let concattenationFunction = lastOrDefault concatter concatterArgs
                in let pureConcatterArgs = removeLast concatterArgs
                in let ((newCallRegister, forParse), modifiedArgs) = mapWithContext mapArgs (callRegister, []) pureConcatterArgs
                in let modifiedArgsStr = map (stringOrEmpty . join . map nameToString . unVar) modifiedArgs
                in let concatterType = Concatter pureConcatter concattenationFunction $ lastOrEmpty modifiedArgsStr
                in Right (
                    Parsed concatterType modifiedArgs modifiedArgsStr,
                    forParse, 
                    newCallRegister
                   )
            


public export
parseArgumentsList : CallRegister -> (callOperatorArgs : List TTImp) -> Errorable (ConcurrentFunction, List TTImp, CallRegister)
parseArgumentsList callRegister [x1, x2] = createConcurrentFunction callRegister x1 x2
parseArgumentsList _            args     = errorExtraList args $ "Argument list of concurrent operator is wrong size: " ++ (show $ length args)