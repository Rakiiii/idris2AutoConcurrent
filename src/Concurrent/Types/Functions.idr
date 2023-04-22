module Concurrent.Types.Functions

import public Concurrent.Utils.IR

import public Language.Reflection
import public Language.Reflection.Types
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Text.PrettyPrint.Bernardy


---------------------------------------------------------- Data types --------------------------------------------------------------------------------
public export
data ResultVariable : Type where
    ResultSaved    : List String -> List (Maybe Name) -> ResultVariable
    ResultNotSaved : ResultVariable

public export
data ArgumentConstructorType = NoConstructor | Pure | 
                                                    -- DslConcatter userCompositionFunction arguments
                                                    Concatter TTImp TTImp String

public export
record InputArgument where
    constructor MkInputArgument
    type : TTImp
    name : String

public export
record ArgumentType where
    constructor MkArgumentType
    type : TTImp

public export
data ArgumentConstructor : Type where
    Parsed : ArgumentConstructorType -> (arguments : List TTImp) -> (argumentNames : List String) -> ArgumentConstructor

public export
record ConcurrentFunction where
    constructor MkConcurrentFunction
    function            : TTImp
    argumentConstructor : ArgumentConstructor

public export
record SplittedFunctionBody where
    constructor MkSplittedFunctionBody
    resultVariable : ResultVariable
    function       : ConcurrentFunction

namespace Typed 
    public export
    record TypedSplittedFunctionBody where
        constructor MkTypedSplittedFunctionBody
        resultVariable : ResultVariable
        function       : ConcurrentFunction
        returnType     : ArgumentType
        inputType      : ArgumentType
----------------------------------------------------------------------------------------------------------------------------------------------------


















--------------------------------------------------------------- Interface Impl ----------------------------------------------------------------------
formatFunction : TTImp -> (Doc opts)
formatFunction = withQuotes . stringOrEmpty . join . map nameToString . unVar

public export
implementation Pretty InputArgument where
    prettyPrec p arg = text "Input argument:" <++> text arg.name <++> text "with type" <++> prettyPrec p arg.type

public export
implementation Pretty ArgumentType where
    prettyPrec p arg = text "type" <++> prettyPrec p arg.type

public export
implementation Pretty ResultVariable where
    prettyPrec p (ResultSaved vars _)       = text $ show vars
    prettyPrec p ResultNotSaved             = empty

public export
implementation Pretty ArgumentConstructor where
    prettyPrec p (Parsed NoConstructor                   _ arguments) = text "wraped variable:" <++> withQuotes (firstOrEmpty arguments)
    prettyPrec p (Parsed Pure                            _ arguments) = text "pure variable:" <++> withQuotes (firstOrEmpty arguments)
    prettyPrec p (Parsed (Concatter dslConcatter f name) _ arguments) = text "concatanation of:" <++> text (show arguments) <++> 
                                                                            text "concatenated by function:" <++> formatFunction dslConcatter <++> 
                                                                                text "user composition function:" <++> formatFunction f     


public export
implementation Pretty ConcurrentFunction where
    prettyPrec p f = formatFunction f.function <++> text "Called with" <++> prettyPrec p f.argumentConstructor <+> ";"   

public export
implementation Pretty SplittedFunctionBody where
    prettyPrec p f = (prettyPrec p f.function) <++> case f.resultVariable of
                                                        ResultNotSaved           => text "Result was not saved" 
                                                        (ResultSaved _ _)        => (text "Result was saved to:" <++> prettyPrec p f.resultVariable)
                                
public export
implementation Pretty TypedSplittedFunctionBody where
        prettyPrec p f = (prettyPrec p f.function) <++> text "Returns" <++> prettyPrec p f.inputType <++> case f.resultVariable of
                                                        ResultNotSaved           => text "Result of" <++> prettyPrec p f.returnType <++> text "was not saved" 
                                                        (ResultSaved _ _)        => (text "Result of" <++> prettyPrec p f.returnType <++> text "was saved to:" <++> prettyPrec p f.resultVariable)
----------------------------------------------------------------------------------------------------------------------------------------------------

















----------------------------------------------------------------------------------------------------------------------------------------------------
implementation Foldable container => Zippable container => Eq a => Eq (container a) where
    (==) = compare

implementation Eq Arg where
    (==) (MkArg c1 p1 n1 t1) (MkArg c2 p2 n2 t2) = c1 == c2 && p1 == p2 && n1 == n2 && t1 == t2

public export
implementation Eq InputArgument where
    (==) a1 a2 = a1.type == a2.type && a1.name == a2.name

public export
implementation Eq ArgumentType where
    (==) a1 a2 = a1.type == a2.type

public export 
implementation Eq ResultVariable where
    (==) (ResultSaved args1 names1)        (ResultSaved args2 names2)        = length args1 == length args2 && compare names1 names2 && compare args1 args2
    (==) ResultNotSaved                    ResultNotSaved                    = True
    (==) _                                 _                                 = False

public export
implementation Eq ArgumentConstructorType where
    (==) NoConstructor     NoConstructor     = True
    (==) Pure              Pure              = True
    (==) (Concatter dsl1 ttimp1 name1) (Concatter dsl2 ttimp2 name2) = dsl1 == dsl2 && ttimp1 == ttimp2 && name1 == name2
    (==) _                 _                 = False

public export 
implementation Eq ArgumentConstructor where
    (==) (Parsed type1 args1 names1) (Parsed type2 args2 names2) = type1 == type2 && length args1 == length args2 && compare args1 args2 && length names1 == length names2 && compare names1 names2

public export
implementation Eq ConcurrentFunction where
    (==) f1 f2 = f1.function == f2.function && f1.argumentConstructor == f2.argumentConstructor

public export
implementation Eq SplittedFunctionBody where
    (==) f1 f2 = f1.function == f2.function && f1.resultVariable == f2.resultVariable

public export
implementation Eq TypedSplittedFunctionBody where
    (== ) f1 f2 = f1.function == f2.function && f1.resultVariable == f2.resultVariable && f1.returnType == f2.returnType && f1.inputType == f2.inputType
----------------------------------------------------------------------------------------------------------------------------------------------------


















----------------------------------------------------------------------------------------------------------------------------------------------------
CHANNEL_NAME_PART = "channel"

public export
interface Channalable a where
    channelName : a -> Name
    channelNameStr : a -> String

public export
implementation Channalable SplittedFunctionBody where
    channelName s = UN $ Basic $ CHANNEL_NAME_PART ++ "_" ++ (stringOrEmpty $ extractMaybeNameString $ s.function.function)
    channelNameStr s = CHANNEL_NAME_PART ++ "_" ++ (stringOrEmpty $ extractMaybeNameString $ s.function.function)

public export
implementation Channalable TypedSplittedFunctionBody where
    channelName s = UN $ Basic $ CHANNEL_NAME_PART ++ "_" ++ (stringOrEmpty $ extractMaybeNameString $ s.function.function)
    channelNameStr s = CHANNEL_NAME_PART ++ "_" ++ (stringOrEmpty $ extractMaybeNameString $ s.function.function)

public export
makeTyped : SplittedFunctionBody -> ArgumentType -> TypedSplittedFunctionBody
makeTyped f a = MkTypedSplittedFunctionBody f.resultVariable f.function a a

public export
makeTypedRaw : SplittedFunctionBody -> TTImp -> TypedSplittedFunctionBody
makeTypedRaw f a = MkTypedSplittedFunctionBody f.resultVariable f.function (MkArgumentType a) (MkArgumentType a)
----------------------------------------------------------------------------------------------------------------------------------------------------