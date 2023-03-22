module Concurrent.Partition.GraphPartitioner

import Concurrent.Types.Functions
import Concurrent.Types.DataFlowGraph
import Concurrent.Utils.IR

import Data.Nat


-----------------------------------------------------------------------------------------------------------------------------------
public export
interface GraphWeightConfig a where
    weightNode : a -> DependentLine -> Weight

public export
interface GraphBiPartitioner a where
    doBiPartition : GraphWeightConfig b => a -> b -> Table DependentLine -> GraphBiPartition OrderedDependentLine
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
implementation GraphWeightConfig TrivaialGraphWeightConfig where
    weightNode (WeightAllNatConfig weight) _ = NatWeight weight
-----------------------------------------------------------------------------------------------------------------------------------







-----------------------------------------------------------------------------------------------------------------------------------
convert : (List DependentLine, List DependentLine) -> (Table OrderedDependentLine, Table OrderedDependentLine)
convert (a, b) = (MkTable $ fst $ foldl addIndex ([], Z) $ reverse a, MkTable $ fst $ foldl addIndex ([], Z) $ reverse b) where
    addIndex : (List OrderedDependentLine, Nat) -> DependentLine -> (List OrderedDependentLine, Nat)
    addIndex (acc, index) newLine = (acc ++ [MkOrderedDependentLine index newLine], S index)

-- разбиение должно быть сбалансированным
-- STUB
public export
implementation GraphBiPartitioner Partitioners where
    doBiPartition (Random seed) _ graph = 
        uncurry MkGraphBiPartition $ 
            convert $
                foldlIndexed (partGraph $ length graph.lines) ([],[]) graph.lines where
                    partGraph : Nat -> (Nat, (List DependentLine, List DependentLine)) -> DependentLine -> (List DependentLine, List DependentLine)
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
simplifyDependencies : GraphBiPartition OrderedDependentLine -> GraphBiPartition OrderedDependentLine
simplifyDependencies (MkGraphBiPartition fsg ssg) =
    MkGraphBiPartition (simplifyDependenciesInternal fsg) (simplifyDependenciesInternal ssg) where
        simplifyDependenciesInternal : Table OrderedDependentLine -> Table OrderedDependentLine
        simplifyDependenciesInternal table = map (filterDependencies table.lines) table where
            filterDependencies : List OrderedDependentLine -> OrderedDependentLine -> OrderedDependentLine
            filterDependencies lines (MkOrderedDependentLine order line) = 
                MkOrderedDependentLine order $
                    MkDependentLine line.function $ filterNot ((flip contains) $ map extractFunction lines) line.dependencies where
                        extractFunction : OrderedDependentLine -> SplittedFunctionBody
                        extractFunction line = line.line.function
            

