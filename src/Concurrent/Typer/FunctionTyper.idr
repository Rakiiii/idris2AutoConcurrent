module Concurrent.Typer.FunctionTyper

import public Concurrent.Types.DataFlowGraph
import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Utils.IR

import public Language.Reflection
import public Language.Reflection.Types
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax

%language ElabReflection

nameOrError : SplittedFunctionBody -> Errorable Name
nameOrError s = maybe (errorExtra s.function.function "Cannot get name for function") Right $ unVar s.function.function

getTypeInternal : Name -> Elab $ List (Name, TTImp)
getTypeInternal = getType

-- tmp
public export
getFunctionType' : SplittedFunctionBody -> Errorable $ Elab TTImp
getFunctionType' s = ((nameOrError s
                        `rxMap` getTypeInternal)
                        `rxMapInternal` map snd)
                        `rxMapInternal` firstOrDefault `(Stub)

public export
getFunctionType'' : SplittedFunctionBody -> Elab TTImp
getFunctionType'' s = do
    let name = maybe (UN $ Basic "stub") id $ unVar s.function.function
    type <- getType name
    let (_, type) = firstOrDefault ((UN $ Basic "stub"), `(STUB)) type
    pure type
    -- ?conv $ (unVar s.function.function
    --     `rxMap` (getType )
    
    -- case maybeName of
    --     Nothing   => do pure error
    --     Just name => do names <- getType name
    --                     case names of
    --                             []              => do pure error
    --                             ((_, type)::xs) => pure $ Right type


    -- fromMaybeName (Just name) = do 
    --                             names <- getType name
    --                             namesToType names
    
    -- fromMaybeName Nothing     = pure $ ?err --error "Cannot get name for function"
    -- let error : Errorable TTImp := errorExtra s.function.function "Cannot get name for function"
    -- case maybeName of
    --     Nothing   => do pure error
    --     Just name => do names <- getType name
    --                     case names of
    --                             []              => do pure error
    --                             ((_, type)::xs) => pure $ Right type

-- public export
getFunctionType :  SplittedFunctionBody -> Elab $ Errorable TTImp
getFunctionType s = do
    let maybeName = unVar s.function.function
    let error : Errorable TTImp := errorExtra s.function.function "Cannot get name for function"
    case maybeName of
        Nothing   => do pure error
        Just name => do names <- getType name
                        case names of
                                []              => do pure error
                                ((_, type)::xs) => pure $ Right type
-- tmp
public export
constructTyped : SplittedFunctionBody -> Elab $ Errorable TypedSplittedFunctionBody
constructTyped func = (getFunctionType func
                            `rxMapInternal` MkArgumentType)
                            `rxMapInternal` (makeTyped func)

public export
findTypesForFunctions : List SplittedFunctionBody -> Elab $ ErrorableList TypedSplittedFunctionBody
findTypesForFunctions [] = pure $ error "Try to get types for empty list of SplittedFunctionBody"
findTypesForFunctions functions = (functions
                                    `rxMap` constructTyped)
                                    `rxJoinListEitherM` ()