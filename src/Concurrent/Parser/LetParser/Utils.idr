module Concurrent.Parser.LetParser.Utils

import public Concurrent.Api.ConcurrentDsl
import public Concurrent.Types.Execution
import public Concurrent.Utils.IR

import public Language.Reflection
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax

%default total
%language ElabReflection

public export
extractCallOperator : TTImp -> TTImp
extractCallOperator = unNamedApp . unNamedApp . fst . unApp . snd . unLambda where 
    unNamedApp : TTImp -> TTImp
    unNamedApp (INamedApp _ func name args) = func
    unNamedApp f = f

public export
-- (call, param name , param type)
unNamedAppUnsafe : TTImp -> (TTImp, Name, TTImp)
unNamedAppUnsafe (INamedApp _ func name args) = (func, name, args)
unNamedAppUnsafe f = (f, UN $ Basic "UNSAFE_UN_APPED", f)

public export
CallOperator : TTImp
CallOperator = extractCallOperator $ %runElab do Reflection.quote $ \x => do stubFunction << x where
    stubFunction : Nat -> Nat
    stubFunction = id


public export
RESULT : String
RESULT = "_result_"


public export
record CallKey where
    constructor MkCallKey
    call : TTImp
    numberOfCalls : Nat
    isFunctionParsed : Bool


public export
appendNumberOfCalls : CallKey -> CallKey
appendNumberOfCalls key = {numberOfCalls := key.numberOfCalls} key 


public export
setParsed : Bool -> CallKey -> CallKey
setParsed isParsed key = {isFunctionParsed := isParsed} key 


public export
record CallRegister where
    constructor MkCallRegister
    calls : List CallKey


public export
map : (List CallKey -> List CallKey) -> CallRegister -> CallRegister
map f (MkCallRegister list) = MkCallRegister $ f list 

-- Этот метод должен обновлять состояние регистра - вызывается перед вложенным вызовом
-- тут возможно не так, тк непонятно правильно ли провходит сравнение
public export
updateRegister : CallRegister -> TTImp -> Bool -> (CallRegister, CallKey)
updateRegister (MkCallRegister register) call isParsed = mapFst MkCallRegister $ updateRegisterInternal register call isParsed where
    updateRegisterInternal : List CallKey -> TTImp -> Bool -> (List CallKey, CallKey)
    updateRegisterInternal [] call isParsed = let key = MkCallKey call 0 isParsed in ([key], key)
    updateRegisterInternal (key::keys) call isParsed = 
            if key.call == call 
                then  let newKey = setParsed isParsed $ appendNumberOfCalls key in (newKey::keys, newKey)
                else  mapFst (\newKeys => key::newKeys) $ updateRegisterInternal keys call isParsed

public export
inRegister : CallRegister -> TTImp -> Maybe CallKey
inRegister (MkCallRegister register) call = inRegisterInternal register call where
    inRegisterInternal : List CallKey -> TTImp -> Maybe CallKey
    inRegisterInternal [] _ = Nothing
    inRegisterInternal (key::keys) call = if call == key.call then Just key else inRegisterInternal keys call



-- Этот метод должен создавать имя переменной которая должна быть около let
-- тк в callKey лежит конструкция вида f << args ... то надо достать f и сгенерировать по ключу имя переменной
public export
generateResultVariableForCall : CallKey -> Errorable TTImp
generateResultVariableForCall key = 
    let (_, args) = unApp key.call
    in case args of 
        (func::_) => Right $ var $ UN $ Basic $ (stringOrEmpty $ join $ nameToStringSep "_" <$> unVar func) ++ RESULT ++ (show key.numberOfCalls)
        _         => errorExtra key.call "Incorrect call construction in CallKey: "

public export
generateResultVariableForCallUnsafe : CallKey -> TTImp
generateResultVariableForCallUnsafe key = 
    let (_, args) = unApp key.call
    in case args of 
        (func::_) => var $ UN $ Basic $ (stringOrEmpty $ join $ nameToStringSep "_" <$> unVar func) ++ RESULT ++ (show key.numberOfCalls)
        _         => key.call