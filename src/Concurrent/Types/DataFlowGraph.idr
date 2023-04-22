module Concurrent.Types.DataFlowGraph

import public Concurrent.Types.Functions
import public Concurrent.Utils.IR

import public Data.Fun
import public Data.List
import public Text.PrettyPrint.Bernardy
import public Language.Reflection

-----------------------------------------------------------------------------------------------------------------------------------
public export
data Weight : Type where
    NatWeight : Nat -> Weight

public export
record DependentLine functionBodyType where
    constructor MkDependentLine
    function     : functionBodyType
    dependencies : List functionBodyType

public export
record Table a where
    constructor MkTable
    lines : List a

public export 
record OrderedDependentLine functionBodyType where
    constructor MkOrderedDependentLine
    order : Nat
    line  : DependentLine functionBodyType

public export 
record CleanDependentLine functionBodyType where
    constructor MkCleanDependentLine
    line                 : DependentLine functionBodyType
    chanelGetDepenencies : List functionBodyType


public export
record GraphBiPartition a where
    constructor MkGraphBiPartition
    firstSubGraph , secondSubGraph : Table a

public export
record Seed where
    constructor MkSeed
    seed : Integer
-----------------------------------------------------------------------------------------------------------------------------------










-----------------------------------------------------------------------------------------------------------------------------------
public export
add : a -> Table a -> Table a
add x = MkTable . add x . lines
-----------------------------------------------------------------------------------------------------------------------------------








-----------------------------------------------------------------------------------------------------------------------------------
dataDependenciesNames : SplittedFunctionBody -> List String
dataDependenciesNames s = 
    case s.resultVariable of
        ResultNotSaved      => []
        ResultSaved names _ => names

textDependencies : List SplittedFunctionBody -> String
textDependencies = show . join . map dataDependenciesNames

dataDependenciesNamesTyped : TypedSplittedFunctionBody -> List String
dataDependenciesNamesTyped s = 
    case s.resultVariable of
        ResultNotSaved      => []
        ResultSaved names _ => names

textDependenciesTyped : List TypedSplittedFunctionBody -> String
textDependenciesTyped = show . join . map dataDependenciesNamesTyped

public export
implementation SplittedFunctionBody => Pretty (DependentLine SplittedFunctionBody) where
    prettyPrec p line = paragraph $ (flush $ text "Function:" <++> prettyPrec p line.function) <+> 
        (text $ "Dependencies: " ++ textDependencies line.dependencies)

public export
implementation TypedSplittedFunctionBody => Pretty (DependentLine TypedSplittedFunctionBody) where
    prettyPrec p line = paragraph $ (flush $ text "Function:" <++> prettyPrec p line.function) <+> 
        (text $ "Dependencies: " ++ textDependenciesTyped line.dependencies)

public export
implementation Pretty a => Pretty (Table a) where
    prettyPrec p table = prettyPrec p table.lines

public export
implementation SplittedFunctionBody => Pretty (OrderedDependentLine SplittedFunctionBody) where
    prettyPrec p (MkOrderedDependentLine ord line) = (flush $ prettyPrec p line) <+> "Call sequence number:" <++> prettyPrec p ord

public export
implementation TypedSplittedFunctionBody => Pretty (OrderedDependentLine TypedSplittedFunctionBody) where
    prettyPrec p (MkOrderedDependentLine ord line) = (flush $ prettyPrec p line) <+> "Call sequence number:" <++> prettyPrec p ord

public export
implementation SplittedFunctionBody => Pretty (CleanDependentLine SplittedFunctionBody) where
    prettyPrec p (MkCleanDependentLine line deps) = (flush $ prettyPrec p line) <+> (text $ "New dependencies for channelGet:" ++ (textDependencies deps))

public export
implementation TypedSplittedFunctionBody => Pretty (CleanDependentLine TypedSplittedFunctionBody) where
    prettyPrec p (MkCleanDependentLine line deps) = (flush $ prettyPrec p line) <+> (text $ "New dependencies for channelGet:" ++ (textDependenciesTyped deps))
-----------------------------------------------------------------------------------------------------------------------------------














-----------------------------------------------------------------------------------------------------------------------------------
public export
implementation Eq functionBodyType => Eq (DependentLine functionBodyType) where
    (==) a b = a.function == b.function && compare a.dependencies b.dependencies

public export
implementation Eq functionBodyType => Eq (OrderedDependentLine functionBodyType) where
    (==) a b = a.line == b.line && a.order == b.order

public export
implementation Eq functionBodyType => Eq (CleanDependentLine functionBodyType) where
    (==) a b = a.line == b.line && compare a.chanelGetDepenencies b.chanelGetDepenencies
    
public export
implementation Eq a => Eq (Table a) where
    (==) a b = compare a.lines b.lines 
-----------------------------------------------------------------------------------------------------------------------------------













-----------------------------------------------------------------------------------------------------------------------------------
public export 
implementation Num Seed where
    (+) s1 s2 = MkSeed (s1.seed + s2.seed)
    (*) s1 s2 = MkSeed (s1.seed * s2.seed)
    fromInteger num = MkSeed num

public export
implementation Num Weight where
    (+) (NatWeight s1) (NatWeight s2) = NatWeight $ s1 + s2
    (*) (NatWeight s1) (NatWeight s2) = NatWeight $ s1 * s2
    fromInteger num = NatWeight $ fromInteger num
-----------------------------------------------------------------------------------------------------------------------------------












-----------------------------------------------------------------------------------------------------------------------------------
public export
interface FunctorT containerStart containerEnd where
    mapT : (Table a -> b) -> containerStart a -> containerEnd b

public export 
implementation FunctorT GraphBiPartition List where
    mapT fun partition = [fun partition.firstSubGraph, fun partition.secondSubGraph]

public export 
implementation FunctorT GraphBiPartition Table where
    mapT fun partition = MkTable [fun partition.firstSubGraph, fun partition.secondSubGraph]

public export
implementation Functor Table where
    map fun table = MkTable $ map fun table.lines

public export
implementation Functor GraphBiPartition where
    map fun partition = MkGraphBiPartition (map fun partition.firstSubGraph) (map fun partition.secondSubGraph) --[fun partition.firstSubGraph, fun partition.secondSubGraph]
-----------------------------------------------------------------------------------------------------------------------------------












-----------------------------------------------------------------------------------------------------------------------------------
public export
implementation Channalable (DependentLine TypedSplittedFunctionBody) where
    channelName = channelName . function
    channelNameStr = channelNameStr . function

public export
implementation Channalable (DependentLine SplittedFunctionBody) where
    channelName = channelName . function
    channelNameStr = channelNameStr . function

public export
implementation Channalable (CleanDependentLine SplittedFunctionBody) where
    channelName = channelName . function . line
    channelNameStr = channelNameStr . function . line

public export
implementation Channalable (CleanDependentLine TypedSplittedFunctionBody) where
    channelName = channelName . function . line
    channelNameStr = channelNameStr . function . line
-----------------------------------------------------------------------------------------------------------------------------------








-----------------------------------------------------------------------------------------------------------------------------------
public export
interface Namable a where
    generateNames : a -> (baseName : String) -> List Name

CONCURRENT_FUNCTION_NAME_POSTFIXS : List String
CONCURRENT_FUNCTION_NAME_POSTFIXS = ["_concurrent_function_1", "_concurrent_function_2"]

public export
implementation Namable (GraphBiPartition a) where 
    generateNames p baseName = (UN . Basic) <$> ((++) baseName) <$> CONCURRENT_FUNCTION_NAME_POSTFIXS
-----------------------------------------------------------------------------------------------------------------------------------










-----------------------------------------------------------------------------------------------------------------------------------
public export
fst : GraphBiPartition a -> Table a
fst = firstSubGraph

public export
snd : GraphBiPartition a -> Table a
snd = secondSubGraph

public export
natWeight : Weight -> Nat
natWeight (NatWeight weight) = weight
-----------------------------------------------------------------------------------------------------------------------------------
