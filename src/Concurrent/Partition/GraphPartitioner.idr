module Concurrent.Partition.GraphPartitioner

import public Concurrent.Types.Functions
import public Concurrent.Types.DataFlowGraph
import public Concurrent.Utils.IR

import public Data.Nat
import public System.Random
import public Data.Fin


-----------------------------------------------------------------------------------------------------------------------------------
%inline
GraphLine : Type
GraphLine = DependentLine TypedSplittedFunctionBody
%inline
OrderedGraphLine : Type
OrderedGraphLine = OrderedDependentLine TypedSplittedFunctionBody

-----------------------------------------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------------------------
public export
interface GraphWeightConfig a b where
    weightNode : a -> DependentLine b -> Weight

public export
interface GraphBiPartitioner a c where
    doBiPartition : GraphWeightConfig b c => a -> b -> Table (DependentLine c) -> GraphBiPartition $ OrderedDependentLine c
-----------------------------------------------------------------------------------------------------------------------------------












-----------------------------------------------------------------------------------------------------------------------------------
public export
data PartitionAlgorithms = Bisection | KerniganeLine

public export
algorithm : a -> a -> PartitionAlgorithms -> a
algorithm b k Bisection = b
algorithm b k KerniganeLine = k

public export
data Partitioners : Type where
    Random : Seed -> Partitioners
    KerniganLine : Seed -> Partitioners

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


------------------------------- Algs ----------------------------------------------------------------------------------------------------
-- tmp 
public export
record WeightedTable a where
    constructor MkWeightedTable
    lines : List (a, Weight)


toTable : WeightedTable a -> Table a
toTable table = MkTable $ map fst table.lines     











-- 
--                                          Алгоритм Кернигана-Лина
--
--  1. Взвесить граф
--  2. Перейти на удобное внутрнее представление
--      Как оно должно выглядеть?
--
--      Что с ним надо будет делать?
--          а) Надо считать разницу при перемещении вершин в другой подграф
--          б) Перемещать вершины в другой подграф
--      
--      Как посчитать (а)?
--          - Надо взять 1 вершину и посмотреть к каким подграфам принадлежат связанные с ней вершины
--          - При перемещении вершин между подграфами - внешние ребра становятся внутреними(кроме 1 ребра между перемещаемыми вершинами), внутренние становятся внешними
--          - [Для связанных вершин] Таким образом надо посчитать : (внешние_ребра_1 - 1) + (внешние_ребра_2 - 1) - внутренние_ребра_1 - внутрение_ребра_2
--          - [Для не связанных вершин] Таким образом надо посчитать : внешние_ребра_1 + внешние_ребра_2 - внутренние_ребра_1 - внутрение_ребра_2
--          - Если коффициент > 0 то надо пермещать
--
--          
--
--
--
--
--          
--
--
--
--
--  3. Сформировать первое решение
--      Как именно? - жадное решение
--  
--  4. Перейти на этап итерационного улучшения
--  
--
--
data Part = Left | Right
implementation Eq Part where
    (==) Left  Left  = True
    (==) Right Right = True
    (==) _     _     = False

not : Part -> Part
not Left  = Right
not Right = Left

record KLGraphNodeNonParted (graphSize : Nat) where
    constructor MkKLGraphNodeNonParted
    index          : Fin graphSize
    connectedNodes : List $ Fin graphSize
    weight         : Nat

record KLGraphNonParted (graphSize : Nat) where
    constructor MkKLGraphNonParted
    nodes : List $ KLGraphNodeNonParted graphSize

record KLGraphNode (graphSize : Nat) where
    constructor MkKLGraphNode
    index          : Fin graphSize
    connectedNodes : List $ Fin graphSize
    externalRibs   : Nat
    internalRibs   : Nat
    weight         : Nat
    part           : Part

record KLGraph (graphSize : Nat) where
    constructor MkKLGraph
    nodes : List $ KLGraphNode graphSize

record MoveCoff (graphSize : Nat) where
    constructor MkMoveCoff
    node  : KLGraphNode graphSize
    coffs : List $ (KLGraphNode graphSize, Integer)

implementation Eq (KLGraphNode graphSize) where
    (==) a b = a.index == b.index

[internalCoffOrd] Ord (KLGraphNode graphSize, Integer) where
    compare a b = compare (snd a) (snd b)    

findLargest : List (KLGraphNode graphSize, Integer) -> Maybe (KLGraphNode graphSize, Integer)
findLargest xs = fst $ sort @{internalCoffOrd} xs where

implementation Eq (MoveCoff graphSize) where
    (==) a b = a.node == b.node && a.coffs == b.coffs

[coffOrd] Ord (MoveCoff graphSize) where
    compare a b = compare (snd <$> findLargest a.coffs) (snd <$> findLargest b.coffs)

isNodesInSamePart : KLGraphNode graphSize -> KLGraphNode graphSize -> Bool
isNodesInSamePart node1 node2 = node1.part == node2.part

decrement : Nat -> Nat
decrement Z = Z
decrement (S n) = n

increment : Nat -> Nat
increment = S

[OrdWithWeights] Eq a => Ord (Pair a Nat) where
    compare a b = compare (snd a) (snd b)

convertToBiPartition : (table : WeightedTable GraphLine) -> KLGraph (length table.lines) -> GraphBiPartition OrderedGraphLine
convertToBiPartition table partition = do 
    let (leftTable, rightTable) = foldl (splitNodes table) ([], []) partition.nodes
    let addOrderToTable = makeOrdered $ lines $ toTable table
    MkGraphBiPartition (MkTable $ addOrderToTable leftTable) (MkTable $ addOrderToTable rightTable) where

    splitNodes :  (table : WeightedTable GraphLine) -> (List GraphLine, List GraphLine) -> KLGraphNode (length $ table.lines) -> (List GraphLine, List GraphLine)
    splitNodes table (left, right) node = do 
        let line = fst $ getAt table.lines node.index 
        let newLeft = if node.part == Left then line::left else left
        let newRight = if node.part == Right then line::right else right
        (newLeft, newRight)

    findLine :  List (Nat, GraphLine) -> GraphLine -> Nat
    findLine [] _ = 0
    findLine ((index,testLine)::xs) line = if line == testLine then index else findLine xs line

    addWeightsToPart : List (Nat, GraphLine) -> GraphLine -> (GraphLine, Nat)
    addWeightsToPart tableWithIndexes line = (line, findLine tableWithIndexes line)

    makeOrdered : (table : List GraphLine) -> (part : List GraphLine) -> List OrderedGraphLine
    makeOrdered table part = do 
        let withIndexes = map (mapFst finToNat) $ toIndexedList table
        let weighetsPart = map (addWeightsToPart withIndexes) part
        let sortByWeights = sort weighetsPart @{OrdWithWeights}
        map (uncurry $ flip MkOrderedDependentLine) sortByWeights

convertToKLGraph : (table : WeightedTable GraphLine) -> KLGraphNonParted $ length table.lines
convertToKLGraph table = MkKLGraphNonParted $ mapIndexed table.lines createKLGraphNode where

    findInTable : (table : WeightedTable GraphLine) -> TypedSplittedFunctionBody -> Maybe $ Fin $ length table.lines
    findInTable (MkWeightedTable []) _ = Nothing
    findInTable (MkWeightedTable (x::xs)) element = if (fst x).function == element then Just 0 else map FS $ findInTable (MkWeightedTable xs) element

    findConnectedNodes : GraphLine -> List $ Fin $ length table.lines
    findConnectedNodes line = catMaybes $ map (findInTable table) line.dependencies

    createKLGraphNode : (Fin $ length (table.lines), (DependentLine TypedSplittedFunctionBody, Weight)) -> KLGraphNodeNonParted $ length table.lines
    createKLGraphNode (index, (line, NatWeight weight)) = MkKLGraphNodeNonParted index (findConnectedNodes line) weight

partWeight : KLGraphNonParted graphSize -> Nat
partWeight graph = half $ sum $ map weight graph.nodes where
    half : Nat -> Nat
    half = div 2

createStartPartition : KLGraphNonParted graphSize -> KLGraph graphSize
createStartPartition (MkKLGraphNonParted []) = MkKLGraph []
createStartPartition graph@(MkKLGraphNonParted (firstNode::xs)) = do 
    let maxWeight = partWeight graph
    let nodeConstructor = uncurry $ choosePartForNode maxWeight
    MkKLGraph $ countRibs $ snd $ mapWithContext nodeConstructor Z graph.nodes where

    findNode : List (KLGraphNode graphSize) -> Fin graphSize -> Maybe $ KLGraphNode graphSize
    findNode [] _ = Nothing
    findNode (node::nodes) nodeIndex = if node.index == nodeIndex then Just node else findNode nodes nodeIndex

    isNodeInSamePart : List (KLGraphNode graphSize) -> KLGraphNode graphSize -> Fin graphSize -> Bool
    isNodeInSamePart nodes node nodeIndex = maybe False id $ map (isNodesInSamePart node) $ findNode nodes nodeIndex

    countRibsForNode : List (KLGraphNode graphSize) -> KLGraphNode graphSize -> KLGraphNode graphSize
    countRibsForNode nodes node = 
        let isInSamePart = isNodeInSamePart nodes node
        in let external : Nat := count (not . isInSamePart) node.connectedNodes
        in let internal : Nat := count isInSamePart node.connectedNodes
        in { externalRibs := external, internalRibs := internal} node

    countRibs : List (KLGraphNode graphSize) -> List $ KLGraphNode graphSize
    countRibs nodes = map (countRibsForNode nodes) nodes

    choosePartForNode : Nat -> Nat -> KLGraphNodeNonParted graphSize -> (Nat, KLGraphNode graphSize)
    choosePartForNode maxWeight leftPartWeight node = do 
        let part = if leftPartWeight >= maxWeight then Right else Left --?choose_node_part
        let newLeftWeight = if part == Left then leftPartWeight + node.weight else leftPartWeight
        pair newLeftWeight $ MkKLGraphNode 
                                node.index
                                node.connectedNodes
                                0
                                0
                                node.weight
                                part


moveNodes : (KLGraphNode graphSize, KLGraphNode graphSize) -> KLGraph graphSize -> KLGraph graphSize
moveNodes (node1, node2) graph = MkKLGraph $ map updateNode graph.nodes where

    moveNode : KLGraphNode graphSize -> KLGraphNode graphSize
    moveNode node = { externalRibs := S node.internalRibs, internalRibs := decrement node.externalRibs, part := not node.part } node

    decrementIfTrue : Nat -> Bool -> Nat
    decrementIfTrue n True  = decrement n
    decrementIfTrue n False = n

    updateExternalRibs : KLGraphNode graphSize -> (c1 : Bool) -> (c2 : Bool) -> Nat
    updateExternalRibs updatingNode c1 c2 = do
        let c1IsSamePart = isNodesInSamePart updatingNode node1
        let c2IsSamePart = isNodesInSamePart updatingNode node2
        let extRibs = updatingNode.externalRibs
        let c1Ribs : Nat := if c1 then if c1IsSamePart then increment extRibs else decrement extRibs else extRibs
        if c2 then if c2IsSamePart then increment c1Ribs else decrement c1Ribs else c1Ribs

    updateInternalRibs : KLGraphNode graphSize -> (c1 : Bool) -> (c2 : Bool) -> Nat
    updateInternalRibs updatingNode c1 c2 = do 
        let c1IsSamePart = isNodesInSamePart updatingNode node1
        let c2IsSamePart = isNodesInSamePart updatingNode node2
        let extRibs = updatingNode.externalRibs
        let c1Ribs : Nat := if c1 then if c1IsSamePart then decrement extRibs else increment extRibs else extRibs
        if c2 then if c2IsSamePart then decrement c1Ribs else increment c1Ribs else c1Ribs
    
    -- надо пересчитать внешние и внутрение ребра в зависимости от наличия связей с перемещаемой парой
    -- возможны 3 случая:
    --          - это перемещаемая нода
    --          - это нода никак не связана с перемещаемыми
    --          - это связанная с перемещаемыми нода
    updateNode : KLGraphNode graphSize -> KLGraphNode graphSize
    updateNode updatingNode = 
        if updatingNode == node1 || updatingNode == node2 
         then moveNode updatingNode
         else 
              let c1 = contains node1.index updatingNode.connectedNodes
              in let c2 = contains node2.index updatingNode.connectedNodes
              in if c1 || c2
                  then { 
                        externalRibs := updateExternalRibs updatingNode c1 c2, 
                        internalRibs := updateInternalRibs updatingNode c1 c2
                       } updatingNode 
                  else updatingNode


isNodesConnected : KLGraphNode graphSize -> KLGraphNode graphSize -> Bool
isNodesConnected node1 node2 = contains node2.index node1.connectedNodes 

countCoffForNode : (currentNode : KLGraphNode graphSize) -> (checkNode : KLGraphNode graphSize) -> (KLGraphNode graphSize, Integer)
countCoffForNode currentNode checkNode = pair checkNode $ countCoffForNodeInternal (isNodesInSamePart currentNode checkNode) (isNodesConnected currentNode checkNode) where
        countCoffForNodeInternal : Bool -> Bool -> Integer
        countCoffForNodeInternal True _     = 0
        countCoffForNodeInternal _    True  = (countCoffForNodeInternal False False) - 2
        countCoffForNodeInternal _    False = (cast (currentNode.externalRibs + checkNode.externalRibs)) - (cast (currentNode.internalRibs + cast checkNode.internalRibs))
  

countMoveCoffsForNode : KLGraphNode graphSize -> KLGraph graphSize -> MoveCoff graphSize
countMoveCoffsForNode node graph = MkMoveCoff node $ map (countCoffForNode node) graph.nodes

countMoveCoffs : KLGraph graphSize -> List $ MoveCoff graphSize
countMoveCoffs (MkKLGraph []) = []
countMoveCoffs (MkKLGraph [x]) = []
countMoveCoffs graph@(MkKLGraph (node::nodes)) = countMoveCoffsInternal graph $ node::nodes where

        countMoveCoffsInternal : KLGraph graphSize -> List (KLGraphNode graphSize) -> List $ MoveCoff graphSize
        countMoveCoffsInternal _ [] = []
        countMoveCoffsInternal graph [node] = [countMoveCoffsForNode node graph]
        countMoveCoffsInternal graph (node::nodes) = (countMoveCoffsForNode node graph)::(countMoveCoffsInternal graph nodes)

findMovePair :  List (MoveCoff graphSize) -> Maybe (KLGraphNode graphSize, KLGraphNode graphSize)
findMovePair xs = 
    case fst $ sort @{coffOrd} xs of
        Nothing => Nothing
        Just x  => 
            case findLargest x.coffs of
                Nothing       => Nothing
                Just (_ , 0)  => Nothing
                Just (n , _)  => Just $ pair x.node n

data IsUpdated = Yes | No
isUpdated : IsUpdated -> a -> a -> a
isUpdated No  y _ = y
isUpdated Yes _ z = z

itterationOfKerniganLine : (partiotion : KLGraph graphSize) -> (KLGraph graphSize, IsUpdated)
itterationOfKerniganLine partition = do 
    let moveCoffs = countMoveCoffs partition
    let movePair = findMovePair moveCoffs
    let flipMove = flip moveNodes
    let maybeNewPartition = map (flipMove partition) movePair
    maybe (partition, No) (dup $ const Yes) maybeNewPartition

itterateOverKerniganLine : (maxAmountOfItterations : Nat) -> (partiotion : KLGraph graphSize) -> KLGraph graphSize
itterateOverKerniganLine Z partiotion = partiotion
itterateOverKerniganLine (S nextMaxAmountOfItteration) partition = 
    let (newPartition, flag) = itterationOfKerniganLine partition
    in isUpdated flag partition $ itterateOverKerniganLine nextMaxAmountOfItteration newPartition













-- tmp
public export
addWeightsToGraph : GraphWeightConfig b TypedSplittedFunctionBody => 
                    (weighter : b)                                -> 
                    (graph : Table GraphLine)                     -> 
                        WeightedTable GraphLine
addWeightsToGraph weighter graph = MkWeightedTable $ map addWeightToLine graph.lines where

    addWeightToLine : GraphLine -> (GraphLine, Weight)
    addWeightToLine = dup $ weightNode weighter

-- tmp
public export
bipartGraphWithKerniganlLine :  GraphWeightConfig b TypedSplittedFunctionBody => 
                                (maxAmountOfItteractions : Nat)               ->
                                (startPartitionGenerator : Seed)              -> 
                                (weighter : b)                                -> 
                                (graph : Table GraphLine)                     -> 
                                    GraphBiPartition OrderedGraphLine
bipartGraphWithKerniganlLine maxAmountOfItteractions startPartitionGenerator weighter graph = do
    let weightedGraph = addWeightsToGraph weighter graph
    let klGraphNonParted = convertToKLGraph weightedGraph
    let startPartition = createStartPartition klGraphNonParted
    let partition = itterateOverKerniganLine maxAmountOfItteractions startPartition
    convertToBiPartition weightedGraph partition























-----------------------------------------------------------------------------------------------------------------------------------
MAX_AMOUNT_OF_ITTERATIONS_KL = S 999


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
    doBiPartition (KerniganLine seed) weighter graph = bipartGraphWithKerniganlLine MAX_AMOUNT_OF_ITTERATIONS_KL seed weighter graph
    doBiPartition (Random seed) _ graph = 
        uncurry MkGraphBiPartition $ 
            convertTyped $
                foldlIndexed (partGraph $ length graph.lines) ([],[]) graph.lines where

                    partGraph : Nat -> (Nat, (List GraphLine, List GraphLine)) -> GraphLine -> (List GraphLine, List GraphLine)
                    partGraph length (index, (first, second)) line = 
                        if index < (div length 2) then ((line::first), second) else (first, (line::second))

-- разбиение должно быть сбалансированным
-- STUB
-- legacy
public export
implementation GraphBiPartitioner Partitioners SplittedFunctionBody where
    doBiPartition (KerniganLine seed) weighter graph = ?unsupported_hole
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
KLBiPartitioner : ?
KLBiPartitioner = KerniganLine 0

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
            

