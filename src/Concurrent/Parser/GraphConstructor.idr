module Concurrent.Parser.GraphConstructor

import Concurrent.Types.Functions
import Concurrent.Types.DataFlowGraph
import Concurrent.Utils.IR

import Data.List
import Data.Maybe

-- Возвращает true если functionForCheck зависимт от resultedFunction
checkDependencie : (functionForCheck : SplittedFunctionBody) -> (resultedFunction : SplittedFunctionBody) -> Bool
checkDependencie originalFunction (MkSplittedFunctionBody ResultNotSaved _) = False
checkDependencie originalFunction (MkSplittedFunctionBody (ResultSaved vars _) _) = 
            let (Parsed _ _ args) = originalFunction.function.argumentConstructor in any ((flip contains) args) vars

-- тут должны проверить все аргументы из функции, на вхлждения в результаты работы остальных функций
findDependencies : SplittedFunctionBody -> List SplittedFunctionBody -> List SplittedFunctionBody
findDependencies function functions = filter (checkDependencie function) functions

-- Метод создания DependentLine
dependentLine : SplittedFunctionBody -> List SplittedFunctionBody -> DependentLine
dependentLine function functions = MkDependentLine function $ findDependencies function functions

-- надо еще отфильтровать одинаковые вхождения в зависимости
-- делаю это выше вроде
public export
constructDataDependencieGraph : List SplittedFunctionBody -> Table DependentLine
constructDataDependencieGraph list = foldl (constructDataFlowGraphInternal list) (MkTable []) list where
    constructDataFlowGraphInternal : List SplittedFunctionBody -> Table DependentLine -> SplittedFunctionBody -> Table DependentLine
    constructDataFlowGraphInternal functions table function = add (dependentLine function functions) table --MkTable $ add (dependentLine function functions) table.lines