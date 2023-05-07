module Concurrent.Parser.GraphConstructor

import public Concurrent.Types.Functions
import public Concurrent.Types.DataFlowGraph
import public Concurrent.Utils.IR

import public Data.List
import public Data.Maybe

-- Возвращает true если functionForCheck зависимт от resultedFunction
checkDependencie : (functionForCheck : TypedSplittedFunctionBody) -> (resultedFunction : TypedSplittedFunctionBody) -> Bool
checkDependencie originalFunction (MkTypedSplittedFunctionBody ResultNotSaved _ _ _) = False
checkDependencie originalFunction (MkTypedSplittedFunctionBody (ResultSaved vars _) _ _ _) = 
            let (Parsed _ _ args) = originalFunction.function.argumentConstructor in any ((flip contains) args) vars

-- тут должны проверить все аргументы из функции, на вхлждения в результаты работы остальных функций
findDependencies : TypedSplittedFunctionBody -> List TypedSplittedFunctionBody -> List TypedSplittedFunctionBody
findDependencies function functions = filter (checkDependencie function) functions

-- Метод создания DependentLine
dependentLine : TypedSplittedFunctionBody -> List TypedSplittedFunctionBody -> DependentLine TypedSplittedFunctionBody
dependentLine function functions = MkDependentLine function $ findDependencies function functions

-- надо еще отфильтровать одинаковые вхождения в зависимости
-- делаю это выше вроде
public export
constructDataDependencieGraph : List TypedSplittedFunctionBody -> Table $ DependentLine TypedSplittedFunctionBody
constructDataDependencieGraph list = foldl (constructDataFlowGraphInternal list) (MkTable []) list where
    constructDataFlowGraphInternal : List TypedSplittedFunctionBody -> Table (DependentLine TypedSplittedFunctionBody) -> TypedSplittedFunctionBody -> Table $ DependentLine TypedSplittedFunctionBody
    constructDataFlowGraphInternal functions table function = add (dependentLine function functions) table 