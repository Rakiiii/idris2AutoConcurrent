module Concurrent.Partition.GraphPartitioner

import public Concurrent.Types.Functions
import public Concurrent.Types.DataFlowGraph
import public Concurrent.Utils.IR

import public Data.Nat


-----------------------------------------------------------------------------------------------------------------------------------
public export
interface GraphWeightConfig a b where
    weightNode : a -> DependentLine b-> Weight

public export
interface GraphBiPartitioner a c where
    doBiPartition : GraphWeightConfig b c => a -> b -> Table (DependentLine c) -> GraphBiPartition $ OrderedDependentLine c
-----------------------------------------------------------------------------------------------------------------------------------












-----------------------------------------------------------------------------------------------------------------------------------
public export
data Partitioners : Type where
    Random : Seed -> Partitioners

public export
data TrivaialGraphWeightConfig : Type where
    WeightAllNatConfig : Nat -> TrivaialGraphWeightConfig
-----------------------------------------------------------------------------------------------------------------------------------








-----------------------------------------------------------------------------------------------------------------------------------
public export
implementation GraphWeightConfig TrivaialGraphWeightConfig TypedSplittedFunctionBody where
    weightNode (WeightAllNatConfig weight) _ = NatWeight weight

public export
implementation GraphWeightConfig TrivaialGraphWeightConfig SplittedFunctionBody where
    weightNode (WeightAllNatConfig weight) _ = NatWeight weight
-----------------------------------------------------------------------------------------------------------------------------------







-----------------------------------------------------------------------------------------------------------------------------------
convertTyped : (List $ DependentLine TypedSplittedFunctionBody, List $ DependentLine TypedSplittedFunctionBody) -> (Table $OrderedDependentLine TypedSplittedFunctionBody, Table $ OrderedDependentLine TypedSplittedFunctionBody)
convertTyped (a, b) = (MkTable $ fst $ foldl addIndex ([], Z) $ reverse a, MkTable $ fst $ foldl addIndex ([], Z) $ reverse b) where
    addIndex : (List $ OrderedDependentLine TypedSplittedFunctionBody, Nat) -> DependentLine TypedSplittedFunctionBody -> (List (OrderedDependentLine TypedSplittedFunctionBody), Nat)
    addIndex (acc, index) newLine = (acc ++ [MkOrderedDependentLine index newLine], S index)

convert : (List $ DependentLine SplittedFunctionBody, List $ DependentLine SplittedFunctionBody) -> (Table $ OrderedDependentLine SplittedFunctionBody, Table $ OrderedDependentLine SplittedFunctionBody)
convert (a, b) = (MkTable $ fst $ foldl addIndex ([], Z) $ reverse a, MkTable $ fst $ foldl addIndex ([], Z) $ reverse b) where
    addIndex : (List $ OrderedDependentLine SplittedFunctionBody, Nat) -> DependentLine SplittedFunctionBody -> (List (OrderedDependentLine SplittedFunctionBody), Nat)
    addIndex (acc, index) newLine = (acc ++ [MkOrderedDependentLine index newLine], S index)

-- разбиение должно быть сбалансированным
-- STUB
public export
implementation GraphBiPartitioner Partitioners TypedSplittedFunctionBody where
    doBiPartition (Random seed) _ graph = 
        uncurry MkGraphBiPartition $ 
            convertTyped $
                foldlIndexed (partGraph $ length graph.lines) ([],[]) graph.lines where

                    partGraph : Nat -> (Nat, (List $ DependentLine TypedSplittedFunctionBody, List $ DependentLine TypedSplittedFunctionBody)) -> DependentLine TypedSplittedFunctionBody -> (List $ DependentLine TypedSplittedFunctionBody, List $ DependentLine TypedSplittedFunctionBody)
                    partGraph length (index, (first, second)) line = 
                        if index < (div length 2) then ((line::first), second) else (first, (line::second))

-- разбиение должно быть сбалансированным
-- STUB
public export
implementation GraphBiPartitioner Partitioners SplittedFunctionBody where
    doBiPartition (Random seed) _ graph = 
        uncurry MkGraphBiPartition $ 
            convert $
                foldlIndexed (partGraph $ length graph.lines) ([],[]) graph.lines where

                    partGraph : Nat -> (Nat, (List $ DependentLine SplittedFunctionBody, List $ DependentLine SplittedFunctionBody)) -> DependentLine SplittedFunctionBody -> (List $ DependentLine SplittedFunctionBody, List $ DependentLine SplittedFunctionBody)
                    partGraph length (index, (first, second)) line = 
                        if index < (div length 2) then ((line::first), second) else (first, (line::second))
-----------------------------------------------------------------------------------------------------------------------------------










-----------------------------------------------------------------------------------------------------------------------------------
public export
RandomBiPartitioner : ?
RandomBiPartitioner = Random 0

public export
WeightAll1 : ?
WeightAll1 = WeightAllNatConfig 1

-- Нужен метод который будет чистить зависимости после разбиения
-- то есть удалять зависимость на текущий подграф
public export
simplifyDependencies : GraphBiPartition (OrderedDependentLine TypedSplittedFunctionBody) -> GraphBiPartition (OrderedDependentLine TypedSplittedFunctionBody)
simplifyDependencies (MkGraphBiPartition fsg ssg) =
    MkGraphBiPartition (simplifyDependenciesInternal fsg) (simplifyDependenciesInternal ssg) where
        simplifyDependenciesInternal : Table (OrderedDependentLine TypedSplittedFunctionBody) -> Table $ OrderedDependentLine TypedSplittedFunctionBody
        simplifyDependenciesInternal table = map (filterDependencies table.lines) table where
            filterDependencies : List (OrderedDependentLine TypedSplittedFunctionBody) -> OrderedDependentLine TypedSplittedFunctionBody -> OrderedDependentLine TypedSplittedFunctionBody
            filterDependencies lines (MkOrderedDependentLine order line) = 
                MkOrderedDependentLine order $
                    MkDependentLine line.function $ filterNot ((flip contains) $ map extractFunction lines) line.dependencies where
                        extractFunction : OrderedDependentLine TypedSplittedFunctionBody -> TypedSplittedFunctionBody
                        extractFunction line = line.line.function
            

