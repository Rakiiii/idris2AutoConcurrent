module Concurrent.Utils.IR

import Data.Maybe

import Language.Reflection
import Language.Reflection.Pretty
import Language.Reflection.Syntax
import Text.PrettyPrint.Bernardy

------------  Utils  ------------------------------------------------------------------------------------------------------------------------------

MONOID_ARROW_NAME : ?
MONOID_ARROW_NAME = ">>="

public export
uncurryTuple : ( a -> b -> c -> d) -> (a, (b,c)) -> d
uncurryTuple f (x1, (x2, x3)) = f x1 x2 x3 


public export
valueOrDefault : a -> Maybe a -> a
valueOrDefault _ (Just v) = v
valueOrDefault v Nothing  = v

public export
nameToString : Name -> Maybe String
nameToString (UN (Basic name)) = Just name
nameToString _                 = Nothing 

public export
stringOrEmpty : Maybe String -> String
stringOrEmpty = fromMaybe ""

public export
extractMaybeNameString : TTImp -> Maybe String
extractMaybeNameString operator = join $ nameToString <$> unVar operator

public export
isMonoidArrowApplication: TTImp -> Bool
isMonoidArrowApplication body = 
    let (probablyMonoidArrow, _) = unApp body
    in let operatorName = extractMaybeNameString probablyMonoidArrow -- join $ nameToString <$> unVar probablyMonoidArrow
    in case operatorName of
            Just name => name == MONOID_ARROW_NAME
            Nothing   => False

public export
list : ((index : Nat) -> a) -> (size : Nat) -> List a
list f Z = [f Z]
list f (S n) = (f (S n))::(list f n)

public export
listStable : a -> (size : Nat) -> List a
listStable elem = list $ const elem

public export
foldlIndexed : Foldable containerType => ((Nat, accType) -> elemType -> accType) -> accType -> containerType elemType -> accType
foldlIndexed f acc foldabled = snd $ foldl (fInternal f) (0, acc) foldabled where
    fInternal : ((Nat, accType) -> elemType -> accType) -> (Nat, accType) -> elemType -> (Nat, accType)
    fInternal fun (index, ac) e = (index + 1, fun (index, ac) e)

public export
contains : Foldable container => Eq a => a -> container a -> Bool
contains element = any ((==) element) 

public export
filterNot : (a -> Bool) -> List a -> List a
filterNot f = filter (\x => not $ f x)

public export
fromList1toList : List1 a -> List a
fromList1toList (x:::xs) = x::xs

public export
add : a -> List a -> List a
add x xs = reverse $ x::(reverse xs)

namespace List1 
    public export
    add : a -> List1 a -> List1 a
    add x xs = reverse $ x:::(reverse $ fromList1toList xs)


public export
removeLast : List a -> List a
removeLast = reverse . drop 1 . reverse

public export
removeLast1 : List1 a -> List a
removeLast1 = removeLast . fromList1toList

public export
lastOrDefault : a -> List a -> a
lastOrDefault def list = last (def:::list)

public export
firstOrDefault : a -> List a -> a
firstOrDefault def = lastOrDefault def . reverse

public export
lastOrEmpty : List String -> String
lastOrEmpty = lastOrDefault ""

public export
firstOrEmpty : List String -> String
firstOrEmpty = firstOrDefault ""

public export
compare : Foldable container => Zippable container => Eq a => container a -> container a -> Bool
compare c1 c2 = all (uncurry (==)) (zip c1 c2)

public export
removeMaybeFromString : Maybe String -> String
removeMaybeFromString (Just str) = str
removeMaybeFromString Nothing    = ""

public export
parseTrivialCallArgument : TTImp -> List String
parseTrivialCallArgument call = 
    let (concatter, arguments) = unApp call
    in map removeMaybeFromString $ filter isJust $ map extractMaybeNameString $ analyzeCallArguments concatter arguments where
    analyzeCallArguments : TTImp -> List TTImp -> List TTImp
    analyzeCallArguments trivialArgument [] = [trivialArgument]
    analyzeCallArguments pureCall [argument] = [argument]
    analyzeCallArguments concatFunction arguments = removeLast arguments




public export
withQuotes : String -> (Doc opts)
withQuotes str = text $ "\"" ++ str ++ "\""

public export
paragraph : {opts : _} -> (Doc opts) -> (Doc opts)
paragraph d = (flush $ flush $ empty) <+> d
----------------------------------------------------------------------------------------------------------------------------------------------------






----------------------------------------------------------------------------------------------------------------------------------------------------
public export
implementation Pretty a => Pretty (List1 a) where
    prettyPrec p = prettyPrec p . fromList1toList
----------------------------------------------------------------------------------------------------------------------------------------------------