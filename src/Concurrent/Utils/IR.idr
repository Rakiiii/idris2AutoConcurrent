module Concurrent.Utils.IR

import public  Data.Maybe
import public  Data.Fin

import public  Language.Reflection
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Text.PrettyPrint.Bernardy

------------  Utils  ------------------------------------------------------------------------------------------------------------------------------

MONOID_ARROW_NAME : ?
MONOID_ARROW_NAME = ">>="

public export
uncurryTuple : ( a -> b -> c -> d) -> (a, (b,c)) -> d
uncurryTuple f (x1, (x2, x3)) = f x1 x2 x3 

public export
flipTuple : ( a -> b -> c -> d) -> c -> b -> a -> d
flipTuple f c b a = f a b c

public export
pair : a -> b -> (a, b)
pair = MkPair

public export
invPair : a -> b -> (b, a)
invPair = flip pair

public export
tuple : a -> b -> c -> (a, b, c)
tuple a b c = (a, b, c)

public export
invTuple : b -> c -> a -> (a, (b, c))
invTuple b c a = (a, (b, c))

public export
valueOrDefault : a -> Maybe a -> a
valueOrDefault _ (Just v) = v
valueOrDefault v Nothing  = v

concatAll : List String -> String -> String
concatAll [] _ = ""
concatAll (x::xs) separator = x ++ separator ++ (concatAll xs separator)

public export
nameToStringSep : String -> Name -> Maybe String
nameToStringSep sep (NS (MkNS namespaces) (UN (Basic name))) = Just $ concatAll namespaces sep ++ name
nameToStringSep _ (UN (Basic name))                          = Just name
nameToStringSep _ _                                          = Nothing 

public export
nameToString : Name -> Maybe String
nameToString = nameToStringSep "."

public export
stringOrEmpty : Maybe String -> String
stringOrEmpty = fromMaybe ""

public export
extractMaybeNameString : TTImp -> Maybe String
extractMaybeNameString operator = join $ nameToStringSep "_" <$> unVar operator

public export
name : String -> Name
name = UN . Basic

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
mapWithContext : ((context, a) -> (context, b)) -> context -> List a -> (context, List b)
mapWithContext func startContext foldable = foldl (contextFunc func) (startContext, []) foldable where
    contextFunc : ((context, a) -> (context, b)) -> (context, List b) -> a -> (context, List b)
    contextFunc f (context, elems) elem = mapSnd (\newElem => elems ++ [newElem]) $ f (context, elem)

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
concat : List a -> List a -> List a
concat = (++)

public export
add : a -> List a -> List a
add x xs = reverse $ x::(reverse xs)

namespace List1 
    public export
    add : a -> List1 a -> List1 a
    add x xs = reverse $ x:::(reverse $ fromList1toList xs)

namespace List
    public export
    wrap : a -> List a
    wrap a = [a]

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
fst : List a -> Maybe a
fst [] = Nothing
fst (x::xs) = Just x

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




------------------------------ Experemental Rx -----------------------------------------------------------------------------------------------------
public export
dup : (a -> b) -> a -> (a, b)
dup f a = (a, f a)

listToInd : (list : List a) -> List $ Fin $ length list
listToInd [] = []
listToInd (x::xs) = 0::(map Data.Fin.FS $ listToInd xs)

public export
toIndexedList : (list : List a) -> List (Fin $ length list, a)
toIndexedList list = zip (listToInd list) list

public export
find : Eq a => (list : List a) -> a -> Maybe $ Fin $ length list
find [] _ = Nothing
find (x::xs) element = if x == element then Just 0 else map FS $ find xs element

public export
getAt : (xs : List a) -> Fin (length xs) -> a
getAt [] n impossible
getAt (x::xs) FZ = x
getAt (x::xs) (FS n) = getAt xs n

public export
mapIndexed : (list : List a) -> ((Fin $ length list, a) -> b) -> List b
mapIndexed list f = map f $ toIndexedList list

public export
mapInternal : Functor f1 => Functor f2 => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
mapInternal f x = map (map f) x

public export
rxMapInternal : Functor f1 => Functor f2 => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
rxMapInternal = flip mapInternal

public export
rxMap : Functor f => f a -> (a -> b) -> f b
rxMap = flip map

public export
mapInternalFst : Functor f1 => Bifunctor f2 => (a -> c) -> f1 (f2 a b) -> f1 (f2 c b)
mapInternalFst f x = map (mapFst f) x

public export
mapInternalSnd : Functor f1 => Bifunctor f2 => (b -> c) -> f1 (f2 a b) -> f1 (f2 a c)
mapInternalSnd f x = map (mapSnd f) x

public export
rxMapInternalFst : Functor f1 => Bifunctor f2 => f1 (f2 a b) -> (a -> c) -> f1 (f2 c b)
rxMapInternalFst x f = map (mapFst f) x

public export
rxMapInternalSnd : Functor f1 => Bifunctor f2 => f1 (f2 a b) -> (b -> c) -> f1 (f2 a c)
rxMapInternalSnd x f = map (mapSnd f) x

public export
rxMapFst : Bifunctor f => f a b -> (a -> c) -> f c b
rxMapFst = flip mapFst

public export
rxMapSnd : Bifunctor f => f a b -> (b -> c) -> f a c
rxMapSnd = flip mapSnd

public export
rxFlatMap : Functor f => f a -> (f a -> b) -> b
rxFlatMap c f = f c 

public export
rxJoin : Monad m => m (m a) -> () -> m a
rxJoin x _ = join x

public export
joinEither : Monad m => m (Either a (m b)) -> m (Either a b)
joinEither xWraped = do 
    x <- xWraped
    case x of
        Left a        => pure $ Left a
        Right bWraped => do
                b <- bWraped
                pure $ Right b

public export
rxJoinEither : Monad m => m (Either a (m b)) -> () -> m (Either a b)
rxJoinEither x _ = joinEither x

public export
joinListEither : List (Either a b) -> Either a (List b)
joinListEither [] = Right []
joinListEither ((Left a)::xs) = Left a
joinListEither ((Right a)::xs) = map ((::) a) $ joinListEither xs

public export
rxJoinListEither : List (Either a b) -> () -> Either a (List b)
rxJoinListEither x _ = joinListEither x

public export
joinListEitherM : Monad m => List ( m (Either a b)) -> m (Either a (List b))
joinListEitherM [] = pure $ Right []
joinListEitherM (xWrap::xs) = do
    x <- xWrap
    case x of 
        Left a => pure $ Left a
        Right b => do
            newXs <- joinListEitherM xs
            pure $ map ((::) b) newXs

public export
rxJoinListEitherM : Monad m => List ( m (Either a b)) -> () -> m (Either a (List b))
rxJoinListEitherM list _ = joinListEitherM list

public export
joinEitherPair : Either a (c, Either a b) -> Either a (c, b)
joinEitherPair (Left a) = Left a
joinEitherPair (Right (_, Left a)) = Left a
joinEitherPair (Right (c, Right b)) = Right (c, b)

public export
rxJoinEitherPair : Either a (c, Either a b) -> () -> Either a (c, b)
rxJoinEitherPair x _ = joinEitherPair x

public export
appendListInPair : List a -> (List a, b) -> (List a, b)
appendListInPair list = mapFst $ concat list

public export
extractMonad : Monad m => Either a (m b) -> m (Either a b)
extractMonad (Left a) = pure $ Left a
extractMonad (Right mb) = do
    b <- mb
    pure $ Right b

public export
rxExtractMonad : Monad m => Either a (m b) -> () -> m (Either a b)
rxExtractMonad x _ = extractMonad x


infixl 10 `rxMapInternal`
infixl 10 `rxMap`
infixl 10 `rxMapInternalFst`
infixl 10 `rxMapInternalSnd`
infixl 10 `rxMapFst`
infixl 10 `rxMapSnd`
infixl 10 `rxFlatMap`
infixl 10 `rxJoin`
infixl 10 `rxJoinEither`
infixl 10 `rxJoinListEither`
infixl 10 `rxJoinListEitherM`
infixl 10 `rxJoinEitherPair`
infixl 10 `rxExtractMonad`
----------------------------------------------------------------------------------------------------------------------------------------------------






----------------------------------------------------------------------------------------------------------------------------------------------------
public export
implementation Pretty a => Pretty (List1 a) where
    prettyPrec p = prettyPrec p . fromList1toList

public export
implementation Semigroup (IO ()) where
    (<+>) a b = do  
        tmpA <- a
        tmpB <- b
        pure ()
----------------------------------------------------------------------------------------------------------------------------------------------------