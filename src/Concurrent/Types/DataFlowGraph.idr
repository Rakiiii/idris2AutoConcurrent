module Concurrent.Types.DataFlowGraph

import Concurrent.Types.Functions
import Concurrent.Utils.IR

import Data.Fun
import Data.List
import Text.PrettyPrint.Bernardy
import Language.Reflection

-----------------------------------------------------------------------------------------------------------------------------------
public export
data Weight : Type where
    NatWeight : Nat -> Weight

public export
record DependentLine where
    constructor MkDependentLine
    function : SplittedFunctionBody
    dependencies : List SplittedFunctionBody

public export
record Table a where
    constructor MkTable
    lines : List a

public export 
record OrderedDependentLine where
    constructor MkOrderedDependentLine
    order : Nat
    line  : DependentLine

public export 
record CleanDependentLine where
    constructor MkCleanDependentLine
    line                 : DependentLine
    chanelGetDepenencies : List SplittedFunctionBody


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

public export
implementation Pretty DependentLine where
    prettyPrec p line = paragraph $ (flush $ text "Function:" <++> prettyPrec p line.function) <+> 
        (text $ "Dependencies: " ++ textDependencies line.dependencies)

public export
implementation Pretty a => Pretty (Table a) where
    prettyPrec p table = prettyPrec p table.lines

public export
implementation Pretty OrderedDependentLine where
    prettyPrec p (MkOrderedDependentLine ord line) = (flush $ prettyPrec p line) <+> "Call sequence number:" <++> prettyPrec p ord

public export
implementation Pretty CleanDependentLine where
    prettyPrec p (MkCleanDependentLine line deps) = (flush $ prettyPrec p line) <+> (text $ "New dependencies for channelGet:" ++ (textDependencies deps))
-----------------------------------------------------------------------------------------------------------------------------------














-----------------------------------------------------------------------------------------------------------------------------------
public export
implementation Eq DependentLine where
    (==) a b = a.function == b.function && compare a.dependencies b.dependencies

public export
implementation Eq OrderedDependentLine where
    (==) a b = a.line == b.line && a.order == b.order

public export
implementation Eq CleanDependentLine where
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
implementation Channalable DependentLine where
    channelName = channelName . function

public export
implementation Channalable CleanDependentLine where
    channelName = channelName . function . line
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
-----------------------------------------------------------------------------------------------------------------------------------
