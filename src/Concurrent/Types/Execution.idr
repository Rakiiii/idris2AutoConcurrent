module Concurrent.Types.Execution

import public Concurrent.Types.Functions
import public Concurrent.Types.DataFlowGraph

import public Language.Reflection
import public Language.Reflection.Pretty
import public Text.PrettyPrint.Bernardy

public export
record Error where
    constructor MkError 
    payload : Maybe $ List TTImp
    msg     : String

public export
implementation Pretty Error where
    prettyPrec p err = (flush $ text $ "Error: " ++ err.msg) <++>
                                case err.payload of
                                        Nothing      => text ""
                                        Just payload => (text "Payload: ") <++> (prettyPrec p payload)




public export
record ArgumentWrapper where
    constructor MkArgumentWrapper
    functionDeclarations : List $ List Decl
    functionNames        : List Name

    startArgument        : InputArgument
    graph                : Table $ DependentLine TypedSplittedFunctionBody



public export
Errorable : Type -> Type
Errorable right = Either Error right

public export
ErrorableList : Type -> Type
ErrorableList right = Errorable $ List right

public export
error : String -> Errorable a
error = Left . (MkError Nothing)

public export
errorExtra : TTImp -> String -> Errorable a
errorExtra payload = Left . (MkError $ Just [payload])

public export
errorExtraList : List TTImp -> String -> Errorable a
errorExtraList payload = Left . (MkError $ Just payload)

public export
concurrentFunction : TTImp -> ArgumentConstructor -> Errorable ConcurrentFunction
concurrentFunction f a = Right $ MkConcurrentFunction f a