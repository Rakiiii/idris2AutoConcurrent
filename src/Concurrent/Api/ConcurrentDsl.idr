module Concurrent.Api.ConcurrentDsl

-----------------------------------------------------------------DSL----------------------------------------------------------------
public export
data ConcurrentWrap : Type -> Type

stub : ConcurrentWrap a -> (a -> ConcurrentWrap b) -> ConcurrentWrap b
stub x f = ?stub_rhs

public export
(>>=) : ConcurrentWrap a -> (a -> ConcurrentWrap b) -> ConcurrentWrap b
(>>=) = stub

public export
concurrentOperator : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b
concurrentOperator f x = ?concurrentOperator_rhs

infixr 10 <<
public export
(<<) : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b
(<<) = concurrentOperator

public export
getArgument: Unit -> ConcurrentWrap a

-------------------CREATE MANY COMBINATIONS OF ARGUMENTS---------------------
public export
concatPure : a -> b -> (a -> b -> c) -> ConcurrentWrap c
concatPure x y f = ?concatPure_rhs

public export
concat1 : ConcurrentWrap a -> b -> (a -> b -> c) -> ConcurrentWrap c
concat1 x y f = ?concat1_rhs

public export
concat2 : ConcurrentWrap a -> ConcurrentWrap b -> (a -> b -> c) -> ConcurrentWrap c
concat2 x y f = ?concat2_rhs

public export
return : a -> ConcurrentWrap a
return x = ?return_rhs

public export
pure : a -> ConcurrentWrap a
pure x = ?pure_rhs