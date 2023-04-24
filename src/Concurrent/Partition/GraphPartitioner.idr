module Concurrent.Partition.GraphPartitioner

import public Concurrent.Types.Functions
import public Concurrent.Types.DataFlowGraph
import public Concurrent.Utils.IR

import public Data.Nat
import public System.Random
import public Data.Fin

%default total

-----------------------------------------------------------------------------------------------------------------------------------
%inline
public export
GraphLine : Type
GraphLine = DependentLine TypedSplittedFunctionBody
%inline
public export
OrderedGraphLine : Type
OrderedGraphLine = OrderedDependentLine TypedSplittedFunctionBody

-----------------------------------------------------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------------------------------------------------
public export
interface GraphWeightConfig a where
    weightNode : a -> GraphLine -> Weight
-----------------------------------------------------------------------------------------------------------------------------------












-----------------------------------------------------------------------------------------------------------------------------------
public export
data TrivaialGraphWeightConfig : Type where
    WeightAllNatConfig : Nat -> TrivaialGraphWeightConfig
-----------------------------------------------------------------------------------------------------------------------------------








-----------------------------------------------------------------------------------------------------------------------------------
public export
implementation GraphWeightConfig TrivaialGraphWeightConfig where
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
-- tmp 
public export
data Part = Left | Right
implementation Eq Part where
    (==) Left  Left  = True
    (==) Right Right = True
    (==) _     _     = False

-- tmp 
public export
not : Part -> Part
not Left  = Right
not Right = Left

-- tmp 
public export
record KLGraphNodeNonParted (graphSize : Nat) where
    constructor MkKLGraphNodeNonParted
    index          : Fin graphSize
    connectedNodes : List $ Fin graphSize
    weight         : Nat
    order          : Nat

-- tmp 
public export
record KLGraphNonParted (graphSize : Nat) where
    constructor MkKLGraphNonParted
    nodes : List $ KLGraphNodeNonParted graphSize

-- tmp 
public export
record KLGraphNode (graphSize : Nat) where
    constructor MkKLGraphNode
    index          : Fin graphSize
    connectedNodes : List $ Fin graphSize
    externalRibs   : Nat
    internalRibs   : Nat
    weight         : Nat
    order          : Nat
    part           : Part

-- tmp 
public export
record KLGraph (graphSize : Nat) where
    constructor MkKLGraph
    nodes : List $ KLGraphNode graphSize

-- tmp 
public export
record MoveCoff (graphSize : Nat) where
    constructor MkMoveCoff
    node  : KLGraphNode graphSize
    coffs : List $ (KLGraphNode graphSize, Integer)









-- tmp
public export
implementation Show Part where
    show Left = "Left"
    show Right = "Right"

-- tmp
public export
implementation Show (MoveCoff a) where
    show coff = "node: " ++ show coff.node.index ++ " :: " ++ (show $ map (mapFst index) coff.coffs)

-- tmp 
public export
implementation Show (KLGraphNode a) where
    show node = 
        "node index: " ++ (show node.index) ++ 
        " order: " ++ (show node.order) ++ 
        " weight: " ++ (show node.weight) ++ 
        " part: " ++ show node.part ++ 
        " external ribs: "++ show node.externalRibs ++ 
        " internal ribs: " ++ show node.internalRibs ++ 
        " connected nodes: " ++ (show node.connectedNodes)

-- tmp 
public export
implementation Show (KLGraph a) where
    show graph = "graph:" ++ show graph.nodes

-- tmp 
public export
implementation Show (KLGraphNodeNonParted a) where
    show node = 
        "node index: " ++ (show node.index) ++ 
        " order: " ++ (show node.order) ++ 
        " weight: " ++ (show node.weight) ++ 
        " connected nodes: " ++ (show node.connectedNodes)

-- tmp 
public export
implementation Show (KLGraphNonParted a) where
    show graph = "graph:" ++ show graph.nodes

-- tmp 
public export
implementation Pretty (KLGraphNodeNonParted a) where
    prettyPrec p node = flush $ text $ "node index: " ++ (show node.index) ++ " weight: " ++ (show node.weight) ++ " connected nodes: " ++ (show node.connectedNodes) --?prettyPrecNode_stub

-- tmp 
public export
implementation Pretty (KLGraphNonParted a) where
    prettyPrec p graph = (flush $ text "graph:") <+> prettyPrec p graph.nodes














-- tmp 
public export
implementation Eq (KLGraphNode graphSize) where
    (==) a b = a.index == b.index

-- tmp 
public export
[internalCoffOrd] Ord (KLGraphNode graphSize, Integer) where
    compare a b = compare (snd a) (snd b)    

-- tmp 
public export
findLargest : List (KLGraphNode graphSize, Integer) -> Maybe (KLGraphNode graphSize, Integer)
findLargest xs = fst $ sort @{internalCoffOrd} xs where

-- tmp 
public export
implementation Eq (MoveCoff graphSize) where
    (==) a b = a.node == b.node && a.coffs == b.coffs

-- tmp 
public export
[coffOrd] Ord (MoveCoff graphSize) where
    compare a b = compare (snd <$> findLargest a.coffs) (snd <$> findLargest b.coffs)

-- tmp 
public export
isNodesInSamePart : KLGraphNode graphSize -> KLGraphNode graphSize -> Bool
isNodesInSamePart node1 node2 = node1.part == node2.part

-- tmp 
public export
decrement : Nat -> Nat
decrement Z = Z
decrement (S n) = n

-- tmp 
public export
increment : Nat -> Nat
increment = S

-- tmp 
public export
[OrdWithWeights] Eq a => Ord (Pair a Nat) where
    compare a b = compare (snd a) (snd b)

-- Алгоритм генерирует пустое разюиение с 1 стороны
-- tmp 
public export
convertToBiPartitionWithTableOrder : (table : WeightedTable GraphLine) -> KLGraph (length table.lines) -> GraphBiPartition OrderedGraphLine
convertToBiPartitionWithTableOrder table partition = do 
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

-- tmp 
public export
convertToBiPartitionWithOptimalOrder : (table : WeightedTable GraphLine) -> KLGraph (length table.lines) -> GraphBiPartition OrderedGraphLine
convertToBiPartitionWithOptimalOrder table partition = do 
    let (leftTable, rightTable) = foldl (splitNodes table) ([], []) partition.nodes
    MkGraphBiPartition (MkTable $ makeOrdered leftTable) (MkTable $ makeOrdered rightTable) where

    splitNodes :  (table : WeightedTable GraphLine) -> (List (GraphLine, Nat), List (GraphLine, Nat)) -> KLGraphNode (length $ table.lines) -> (List (GraphLine, Nat), List (GraphLine, Nat))
    splitNodes table (left, right) node = do 
        let line = fst $ getAt table.lines node.index 
        let lineWithOrder = (line, node.order)
        let newLeft = if node.part == Left then lineWithOrder::left else left
        let newRight = if node.part == Right then lineWithOrder::right else right
        (newLeft, newRight)

    findLine :  List (Nat, GraphLine) -> GraphLine -> Nat
    findLine [] _ = 0
    findLine ((index,testLine)::xs) line = if line == testLine then index else findLine xs line

    addWeightsToPart : List (Nat, GraphLine) -> GraphLine -> (GraphLine, Nat)
    addWeightsToPart tableWithIndexes line = (line, findLine tableWithIndexes line)

    makeOrdered : (part : List (GraphLine, Nat)) -> List OrderedGraphLine
    makeOrdered part = do 
        let sortByWeights = sort part @{OrdWithWeights}
        map (uncurry $ flip MkOrderedDependentLine) sortByWeights













-- tmp
public export 
addCrossRibs : KLGraphNonParted graphSize -> KLGraphNonParted graphSize
addCrossRibs graph = { nodes := map addCrossRibsToNode graph.nodes } graph where

    addIfConnected : Fin graphSize -> List (Fin graphSize) -> KLGraphNodeNonParted graphSize -> List (Fin graphSize)
    addIfConnected index acc node = if contains index node.connectedNodes then node.index::acc else acc 

    findAllConnectedNodes : KLGraphNodeNonParted graphSize -> List $ Fin graphSize
    findAllConnectedNodes node = foldl (addIfConnected node.index) [] graph.nodes

    addCrossRibsToNode : KLGraphNodeNonParted graphSize -> KLGraphNodeNonParted graphSize
    addCrossRibsToNode node = { connectedNodes := sort (node.connectedNodes ++ findAllConnectedNodes node)} node --?addCrossRibsToNode_rhs

-- tmp 
public export
convertToKLGraph : (table : WeightedTable GraphLine) -> KLGraphNonParted $ length table.lines
convertToKLGraph table = MkKLGraphNonParted $ mapIndexed table.lines createKLGraphNode where
    
    findInTableInternal : (lines : List (GraphLine, Weight)) -> TypedSplittedFunctionBody -> Maybe $ Fin $ length lines
    findInTableInternal [] _ = Nothing
    findInTableInternal (x::xs) element = if (fst x).function == element then Just 0 else map FS $ findInTableInternal xs element

    findInTable : (table : WeightedTable GraphLine) -> TypedSplittedFunctionBody -> Maybe $ Fin $ length table.lines
    findInTable (MkWeightedTable lines) element = findInTableInternal lines element

    findConnectedNodes : GraphLine -> List $ Fin $ length table.lines
    findConnectedNodes line = catMaybes $ map (findInTable table) line.dependencies

    createKLGraphNode : (Fin $ length (table.lines), (DependentLine TypedSplittedFunctionBody, Weight)) -> KLGraphNodeNonParted $ length table.lines
    createKLGraphNode (index, (line, NatWeight weight)) = MkKLGraphNodeNonParted index (findConnectedNodes line) weight 0

-- tmp
public export 
tryToAddOptimalOrderByDeadline : (maxAmountOfSteps : Nat)-> KLGraphNonParted graphSize -> (KLGraphNonParted graphSize, Bool)
tryToAddOptimalOrderByDeadline Z graph = (graph, False)
tryToAddOptimalOrderByDeadline (S maxAmountOfSteps) graph = 
    if isNodesWithoutOrderExist graph
     then tryToAddOptimalOrderByDeadline maxAmountOfSteps $ addOrderToNodeIfNeeded graph
     else (graph, True) where

    isNodesWithoutOrderExist : KLGraphNonParted graphSize -> Bool
    isNodesWithoutOrderExist graph = any (((==) 0) . order) graph.nodes

    checkOrderIsNotZero : (graph : List $ KLGraphNodeNonParted graphSize) -> (connectedNode : Fin graphSize) -> Bool
    checkOrderIsNotZero []      connectedNode  = True
    checkOrderIsNotZero (x::xs) connectedNode  = 
        if x.index == connectedNode 
         then x.order /= 0 
         else checkOrderIsNotZero xs connectedNode

    isAllConnectedNodesMarked : (graph : KLGraphNonParted graphSize) -> (connectedNodes : List $ Fin graphSize) -> Bool
    isAllConnectedNodesMarked graph connectedNodes = all (checkOrderIsNotZero graph.nodes) connectedNodes

    markNodeWithOrderIfAllConnectedNodeAreMarked : (orderMark : Nat) -> (graph : KLGraphNonParted graphSize) -> (node : KLGraphNodeNonParted graphSize) -> KLGraphNodeNonParted graphSize
    markNodeWithOrderIfAllConnectedNodeAreMarked orderMark graph node = 
        if node.order == 0 && (isNil node.connectedNodes || isAllConnectedNodesMarked graph node.connectedNodes)
         then { order := orderMark } node
         else node 

    findMaxOrder : List (KLGraphNodeNonParted graphSize) -> Nat -> Nat
    findMaxOrder []      currentMax = currentMax
    findMaxOrder (x::xs) currentMax = if x.order > currentMax then findMaxOrder xs x.order else findMaxOrder xs currentMax --?findMaxOrder_rhs

    addOrderToNodeIfNeeded : KLGraphNonParted graphSize -> KLGraphNonParted graphSize
    addOrderToNodeIfNeeded graph = do 
        let maxCurrentOrder = findMaxOrder graph.nodes Z
        let nextOrder = increment maxCurrentOrder
        MkKLGraphNonParted $ map (markNodeWithOrderIfAllConnectedNodeAreMarked nextOrder graph) graph.nodes

-- надо уменьшить колличество просмотров списка вершин
-- что можно обрезать ?
-- + Максимальный номер
-- + Перепроверка списка на предмет его не размеченности
-- Можно уменьшить на 1 обход если считать сколько вершин размечено
-- Поиск вершин для разметки?????
-- tmp
public export 
tryToAddOptimalOrderByDeadlineFast : (maxAmountOfSteps : Nat)-> KLGraphNonParted graphSize -> (KLGraphNonParted graphSize, Bool)
tryToAddOptimalOrderByDeadlineFast maxAmountOfSteps graph = extractResult $ tryToAddOptimalOrderByDeadlineFastInternal True 1 maxAmountOfSteps graph.nodes where

    extractResult : (List (KLGraphNodeNonParted graphSize), (Bool, (Nat, Bool))) -> (KLGraphNonParted graphSize, Bool)
    extractResult (nodes, (_, (_ , isOptimalOrderFound))) = (MkKLGraphNonParted nodes, isOptimalOrderFound)

    isNodesWithoutOrderExist : KLGraphNonParted graphSize -> Bool
    isNodesWithoutOrderExist graph = any (((==) 0) . order) graph.nodes

    checkOrderIsNotZero : (graph : List $ KLGraphNodeNonParted graphSize) -> (connectedNode : Fin graphSize) -> Bool
    checkOrderIsNotZero []      connectedNode  = True
    checkOrderIsNotZero (x::xs) connectedNode  = 
        if x.index == connectedNode 
         then x.order /= 0 
         else checkOrderIsNotZero xs connectedNode

    isAllConnectedNodesMarked : (nodes : List $ KLGraphNodeNonParted graphSize) -> (connectedNodes : List $ Fin graphSize) -> Bool
    isAllConnectedNodesMarked nodes connectedNodes = all (checkOrderIsNotZero nodes) connectedNodes

    markNodeWithOrderIfAllConnectedNodeAreMarkedFast : (orderMark : Nat) -> (nodes : List $ KLGraphNodeNonParted graphSize) -> (node : KLGraphNodeNonParted graphSize) -> (acc : (List $ KLGraphNodeNonParted graphSize, Bool)) ->(List $ KLGraphNodeNonParted graphSize, Bool)
    markNodeWithOrderIfAllConnectedNodeAreMarkedFast orderMark nodes node (acc, flag) = 
        if node.order == 0 && (isNil node.connectedNodes || isAllConnectedNodesMarked nodes node.connectedNodes)
         then (({ order := orderMark } node)::acc, True)
         else (node::acc, flag)

    addOrderToNodeIfNeededFast : Nat -> List (KLGraphNodeNonParted graphSize) -> (List $ KLGraphNodeNonParted graphSize, Bool)
    addOrderToNodeIfNeededFast nextOrder nodes = foldr (markNodeWithOrderIfAllConnectedNodeAreMarkedFast nextOrder nodes) ([], False) nodes

    tryToAddOptimalOrderByDeadlineFastInternal : (shouldBeModified : Bool) -> (orderOnPrevItter : Nat) -> (maxAmountOfSteps : Nat)-> (List $ KLGraphNodeNonParted graphSize) -> (List $ KLGraphNodeNonParted  graphSize, Bool, Nat, Bool)
    tryToAddOptimalOrderByDeadlineFastInternal False orderOnPrevItter _                    nodes = (nodes, False, orderOnPrevItter, True)
    tryToAddOptimalOrderByDeadlineFastInternal True  orderOnPrevItter Z                    nodes = (nodes, False, orderOnPrevItter, False)
    tryToAddOptimalOrderByDeadlineFastInternal _     orderOnPrevItter (S maxAmountOfSteps) nodes = 
        let (suborderedGraph, isSomethingForModifyExist) = addOrderToNodeIfNeededFast orderOnPrevItter nodes
        in tryToAddOptimalOrderByDeadlineFastInternal isSomethingForModifyExist (increment orderOnPrevItter) maxAmountOfSteps suborderedGraph








-- tmp 
public export
partWeight : KLGraphNonParted graphSize -> Nat
partWeight graph = half $ sum $ map weight graph.nodes where
    half : Nat -> Nat
    half = (flip div) 2

-- tmp 
public export
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
                                node.order
                                part


-- tmp 
public export
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


-- tmp 
public export
isNodesConnected : KLGraphNode graphSize -> KLGraphNode graphSize -> Bool
isNodesConnected node1 node2 = contains node2.index node1.connectedNodes 

-- tmp 
public export
countCoffForNode : (currentNode : KLGraphNode graphSize) -> (checkNode : KLGraphNode graphSize) -> (KLGraphNode graphSize, Integer)
countCoffForNode currentNode checkNode = pair checkNode $ countCoffForNodeInternal (isNodesInSamePart currentNode checkNode) (isNodesConnected currentNode checkNode) where
        countCoffForNodeInternal : Bool -> Bool -> Integer
        countCoffForNodeInternal True _     = 0
        countCoffForNodeInternal _    True  = ((cast (currentNode.externalRibs + checkNode.externalRibs)) - (cast (currentNode.internalRibs + cast checkNode.internalRibs))) - 2
        countCoffForNodeInternal _    False = (cast (currentNode.externalRibs + checkNode.externalRibs)) - (cast (currentNode.internalRibs + cast checkNode.internalRibs))
  

-- tmp 
public export
countMoveCoffsForNode : KLGraphNode graphSize -> KLGraph graphSize -> MoveCoff graphSize
countMoveCoffsForNode node graph = MkMoveCoff node $ map (countCoffForNode node) graph.nodes

-- tmp 
public export
countMoveCoffs : KLGraph graphSize -> List $ MoveCoff graphSize
countMoveCoffs (MkKLGraph []) = []
countMoveCoffs (MkKLGraph [x]) = []
countMoveCoffs graph@(MkKLGraph (node::nodes)) = countMoveCoffsInternal graph $ node::nodes where

        countMoveCoffsInternal : KLGraph graphSize -> List (KLGraphNode graphSize) -> List $ MoveCoff graphSize
        countMoveCoffsInternal _ [] = []
        countMoveCoffsInternal graph [node] = [countMoveCoffsForNode node graph]
        countMoveCoffsInternal graph (node::nodes) = (countMoveCoffsForNode node graph)::(countMoveCoffsInternal graph nodes)

-- не учитывает веса разбиения
--  он проблема не тут
-- tmp 
public export
findMovePair :  List (MoveCoff graphSize) -> Maybe (KLGraphNode graphSize, KLGraphNode graphSize)
findMovePair xs = 
    -- тут должен быть не fst, а первая пара с ~ равным весом
    case fst $ sort @{coffOrd} xs of
        Nothing => Nothing
        Just x  => 
            case findLargest x.coffs of
                Nothing          => Nothing
                Just (_ , 0)     => Nothing
                Just (n , coff)  => if coff > 0 then Just $ pair x.node n else Nothing

-- tmp 
public export
data IsUpdated = Yes | No
isUpdated : IsUpdated -> a -> a -> a
isUpdated No  y _ = y
isUpdated Yes _ z = z

-- tmp 
public export
itterationOfKerniganLine : (partiotion : KLGraph graphSize) -> (KLGraph graphSize, IsUpdated)
itterationOfKerniganLine partition = do 
    let moveCoffs = countMoveCoffs partition
    let movePair = findMovePair moveCoffs
    let flipMove = flip moveNodes
    let maybeNewPartition = map (flipMove partition) movePair
    maybe (partition, No) (dup $ const Yes) maybeNewPartition

-- tmp 
public export
itterateOverKerniganLine : (maxAmountOfItterations : Nat) -> (partiotion : KLGraph graphSize) -> KLGraph graphSize
itterateOverKerniganLine Z partiotion = partiotion
itterateOverKerniganLine (S nextMaxAmountOfItteration) partition = 
    let (newPartition, flag) = itterationOfKerniganLine partition
    in isUpdated flag partition $ itterateOverKerniganLine nextMaxAmountOfItteration newPartition

-- tmp
public export
addWeightsToGraph : GraphWeightConfig b                           => 
                    (weighter : b)                                -> 
                    (graph : Table GraphLine)                     -> 
                        WeightedTable GraphLine
addWeightsToGraph weighter graph = MkWeightedTable $ map addWeightToLine graph.lines where

    addWeightToLine : GraphLine -> (GraphLine, Weight)
    addWeightToLine = dup $ weightNode weighter





-----------------------------------------------------------------------------------------------------------------------------------
-- tmp
public export
bipartGraphWithKerniganlLine :  GraphWeightConfig b                           => 
                                (maxAmountOfItteractions : Nat)               ->
                                (maxOrdItteration : Nat)                      ->
                                (weighter : b)                                -> 
                                (graph : Table GraphLine)                     -> 
                                    GraphBiPartition OrderedGraphLine
bipartGraphWithKerniganlLine maxAmountOfItteractions maxOrdItteration weighter graph = do
    let weightedGraph = addWeightsToGraph weighter graph
    let klGraphNonPartedNoCrossRibsNotOrdered = convertToKLGraph weightedGraph
    let (klGraphNonPartedNoCrossRibs, isGraphOptimalOrderWasFound) = tryToAddOptimalOrderByDeadlineFast maxOrdItteration klGraphNonPartedNoCrossRibsNotOrdered--tryToAddOptimalOrderByDeadline 1 klGraphNonPartedNoCrossRibsNotOrdered
    let klGraphNonParted = addCrossRibs klGraphNonPartedNoCrossRibs
    let startPartition = createStartPartition klGraphNonParted
    let partition = itterateOverKerniganLine maxAmountOfItteractions startPartition
    -- TODO:: Добавлять порядок вызова функций на основе klGraphNonPartedNoCrossRibs
    if isGraphOptimalOrderWasFound 
     then convertToBiPartitionWithOptimalOrder weightedGraph partition
     else convertToBiPartitionWithTableOrder weightedGraph partition
-----------------------------------------------------------------------------------------------------------------------------------











-----------------------------------------------------------------------------------------------------------------------------------
convertTyped : (List $ DependentLine TypedSplittedFunctionBody, List $ DependentLine TypedSplittedFunctionBody) -> (Table $OrderedDependentLine TypedSplittedFunctionBody, Table $ OrderedDependentLine TypedSplittedFunctionBody)
convertTyped (a, b) = (MkTable $ fst $ foldl addIndex ([], Z) $ reverse a, MkTable $ fst $ foldl addIndex ([], Z) $ reverse b) where
    addIndex : (List $ OrderedDependentLine TypedSplittedFunctionBody, Nat) -> DependentLine TypedSplittedFunctionBody -> (List (OrderedDependentLine TypedSplittedFunctionBody), Nat)
    addIndex (acc, index) newLine = (acc ++ [MkOrderedDependentLine index newLine], S index)

bisectionPartition : GraphWeightConfig b                           => 
                     (weighter : b)                                -> 
                     (graph : Table GraphLine)                     -> 
                        GraphBiPartition OrderedGraphLine
bisectionPartition _ graph = 
        uncurry MkGraphBiPartition $ 
            convertTyped $
                foldlIndexed (partGraph $ length graph.lines) ([],[]) graph.lines where

                    partGraph : Nat -> (Nat, (List GraphLine, List GraphLine)) -> GraphLine -> (List GraphLine, List GraphLine)
                    partGraph length (index, (first, second)) line = 
                        if index < (div length 2) then ((line::first), second) else (first, (line::second))
-----------------------------------------------------------------------------------------------------------------------------------

























-----------------------------------------------------------------------------------------------------------------------------------
public export
WeightAll1 : ?
WeightAll1 = WeightAllNatConfig 1

export
record Partitioner a where
    constructor WrapFunction
    func : GraphWeightConfig a => a -> Table GraphLine -> GraphBiPartition OrderedGraphLine

public export
findGraphPartition : GraphWeightConfig a => Partitioner a -> a -> Table GraphLine -> GraphBiPartition OrderedGraphLine
findGraphPartition f = f.func

public export
BisectionPartitioner : Partitioner a
BisectionPartitioner = WrapFunction bisectionPartition

public export
KerniganLinParitioner : Nat -> Nat -> Partitioner a
KerniganLinParitioner a b = WrapFunction $ bipartGraphWithKerniganlLine a b

-- Нужен метод который будет чистить зависимости после разбиения
-- то есть удалять зависимость на текущий подграф
public export
simplifyDependencies : GraphBiPartition OrderedGraphLine -> GraphBiPartition OrderedGraphLine
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
            

