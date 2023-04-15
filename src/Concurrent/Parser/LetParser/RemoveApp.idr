module Concurrent.Parser.LetParser.RemoveApp


import public Concurrent.Parser.LetParser.Utils
import public Concurrent.Parser.LetParser.ParseArgumentsList
import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Utils.IR

import public Language.Reflection
import public Language.Reflection.Syntax

%default total
%language ElabReflection

-- надо возвращать еще и CallRegister для рекурсивного вызова
-- надо понять почему функции в регистре помещаются раньше времени (или помещаются вовремя, но надо проверять на парсинг)
covering
public export
removeApp : (saveResult : Bool) -> (callRegister : CallRegister) -> (body : TTImp) -> Errorable (List TypedSplittedFunctionBody, CallRegister)
removeApp saveResult callRegister body = 
        case body of 
            IApp _ _ _ => let (expectedCallOperator, callOperatorArgs) = unApp body 
                          in if isCallOperatorFunc expectedCallOperator 
                            -- Тут надо пропарсить текущую строку и добавить в список следующие строки
                            then 
                                let returnType = extractFunctionReturnTypeFromCallOperator expectedCallOperator
                                in let inputType = extractFunctionInputTypeFromCallOperator expectedCallOperator
                                in let maybeKey = inRegister callRegister body
                                in case maybeKey of
                                    -- тут надо рассмотерть два случая:
                                    --           первый - тривиальный функция учтена в генерации
                                    --           второй - когда не пропарсили и надо пропарсить
                                    Just key => if key.isFunctionParsed 
                                                    -- тривиальный случай когда аргумент уже учтен
                                                  then Right $ ([], callRegister)
                                                    -- случай когда парсим внутренний аргумент-оператор
                                                  else 
                                                    let (newRegister, _) = updateRegister callRegister body True
                                                    in 
                                                        parseArgumentsList newRegister callOperatorArgs
                                                            `rxMap` uncurryTuple (checkInternalArgs returnType inputType saveResult body)
                                                            `rxJoin` ()
                                    Nothing  =>
                                                -- тут надо сформировать строку для сохраненной переменной (Возможно стоит делать до парса аргументов)
                                                -- а потом вызвать рекурсивно на списке полученном из парса
                                                let (newRegister, _) = updateRegister callRegister body True
                                                in 
                                                    parseArgumentsList newRegister callOperatorArgs
                                                        `rxMap` uncurryTuple (checkInternalArgs returnType inputType saveResult body)
                                                        `rxJoin` ()

                            else errorExtra expectedCallOperator "Try to parse not << operator, but << was expected"

            unexpected => errorExtra unexpected "try to remove App, from position when it's not app. Found: "  where

-- Must be refactored
                                extractFunctionReturnTypeFromCallOperator : TTImp -> ArgumentType
                                extractFunctionReturnTypeFromCallOperator = MkArgumentType . snd . snd . unNamedAppUnsafe . fst . unApp
                                
                                extractFunctionInputTypeFromCallOperator : TTImp -> ArgumentType
                                extractFunctionInputTypeFromCallOperator = MkArgumentType . snd . snd . unNamedAppUnsafe . fst . unNamedAppUnsafe . fst . unApp

                                removeAppList : TTImp -> CallRegister -> List TTImp -> Errorable (List TypedSplittedFunctionBody, CallRegister)
                                removeAppList payload register []      = errorExtra payload "removeAppList cannot parse anything with empty list"
                                removeAppList payload register (x::[]) = removeApp True register x
                                removeAppList payload register (x::xs) = removeAppList x register xs
                                                                            `rxMap` (\(list, newRegister) => 
                                                                                            removeApp True newRegister x
                                                                                                `rxMap` appendListInPair list)
                                    
                                                                            `rxJoin` ()
                                -- join $ map (\(list, newRegister) => map (\(xs, lastRegister) => (list ++ xs, lastRegister)) $ removeApp newRegister x) $ removeAppList x register xs

                                isCallOperatorFunc : TTImp -> Bool
                                isCallOperatorFunc actualFunc = (extractCallOperator actualFunc) == CallOperator

                                doSaveResult : (saveResult : Bool) -> (callKey : CallKey) -> Errorable ResultVariable
                                doSaveResult False _      = Right $ ResultNotSaved
                                doSaveResult True callKey = 
                                    (((generateResultVariableForCall callKey
                                        `rxMap` unVar)
                                        `rxMap` (\var => (stringOrEmpty $ join $ nameToString <$> var,var)))
                                        `rxMap` bimap wrap wrap)
                                        `rxMap` (uncurry ResultSaved)
                                    

                                mapNestedFunctionsToSplittedFunc : (returnType : ArgumentType) -> (inputType : ArgumentType) -> (func : ConcurrentFunction) -> (saveResult : ResultVariable) -> Errorable (List TypedSplittedFunctionBody, CallRegister) -> Errorable (List TypedSplittedFunctionBody, CallRegister)
                                mapNestedFunctionsToSplittedFunc  returnType inputType func saveResult nestedFunctions = map (mapFst ((::) (MkTypedSplittedFunctionBody saveResult func returnType inputType))) nestedFunctions

                                mapInternalArgsToSplittedFunc : (returnType : ArgumentType) -> (inputType : ArgumentType) -> (func : ConcurrentFunction) -> CallRegister  -> (saveResult : ResultVariable) -> (List TypedSplittedFunctionBody, CallRegister)
                                mapInternalArgsToSplittedFunc returnType inputType func callRegister saveResult = ([MkTypedSplittedFunctionBody saveResult func returnType inputType ], callRegister)

    
                                checkInternalArgs : (returnType : ArgumentType) -> (inputType : ArgumentType) -> (saveResult : Bool) -> (body : TTImp) -> (func : ConcurrentFunction) -> (nested : List TTImp) -> (callRegister : CallRegister) -> Errorable (List TypedSplittedFunctionBody, CallRegister)
                                checkInternalArgs returnType inputType saveResult body func [] callRegister = 
                                    case inRegister callRegister body of   
                                        Nothing  => errorExtra body "Try to check internal args for function that is not in register" 
                                        Just key => doSaveResult saveResult key
                                                        `rxMap` mapInternalArgsToSplittedFunc returnType inputType func callRegister
 
                                checkInternalArgs returnType inputType saveResult body func nested callRegister = 
                                    case inRegister callRegister body of
                                        Nothing  => errorExtra body "Try to check internal args for function that is not in register"
                                        Just key => doSaveResult saveResult key
                                                        `rxMap` (\resultVar => mapNestedFunctionsToSplittedFunc returnType inputType func resultVar $ removeAppList body callRegister nested)
                                                        `rxJoin` ()
                                                        
                                