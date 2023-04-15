module Concurrent.Parser.LetParser.Api


import public Concurrent.Parser.LetParser.Utils
import public Concurrent.Parser.LetParser.RemoveApp
import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Utils.IR

import public Language.Reflection
import public Language.Reflection.Syntax

%default total
%language ElabReflection

--tmp public export
-- надо достать тип перменной из аргументов и его тоже вернуть
removeLambda : TTImp -> Errorable (List Arg, TTImp)
removeLambda s = case s of
                    ILam _ _ _ _ _ _ => Right $ unLambda s
                    unexpected       => errorExtra unexpected "Function is not lambda^ but lambda expected. Found: "

argumentType : List Arg -> Errorable InputArgument
argumentType [] = error "Lambda function does not contains argument some how"
argumentType [MkArg _ _ maybeName type] = 
    let (errorPayload, expectedRealType) = unApp type
    in let name = maybeName 
                    `rxMap` nameToString 
                    `rxJoin` ()
                    `rxFlatMap` stringOrEmpty                   
    in case expectedRealType of
        [realType] => Right $ MkInputArgument realType name
        _          => errorExtra errorPayload "Incorrect type in lambda function arg"
argumentType _ = error "Lambda function contains too many args"
  
--tmp
covering 
public export
splitFunctionByLetFromLambda : (callRegister : CallRegister) -> (expectLambda : TTImp) -> Errorable (List TypedSplittedFunctionBody, InputArgument)
splitFunctionByLetFromLambda callRegister expectLambda = join $ map functor $ removeLambda expectLambda where

        functor : (List Arg, TTImp) ->  Errorable (List TypedSplittedFunctionBody, InputArgument)
        functor (args, body) = removeApp False callRegister body
                                    `rxMap` fst
                                    `rxMap` (\list => map (pair list) $ argumentType args)
                                    `rxJoin` ()


covering 
public export
parseLambdaFunction : (expectLambda : TTImp) -> Errorable (List TypedSplittedFunctionBody, InputArgument)
parseLambdaFunction = splitFunctionByLetFromLambda (MkCallRegister [])