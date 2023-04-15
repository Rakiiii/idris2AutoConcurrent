module Concurrent.Api.ConcurrentDsl

-----------------------------------------------------------------DSL----------------------------------------------------------------
public export
data ConcurrentWrap : Type -> Type where

public export
concurrentOperator : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b

infixr 10 <<
public export
(<<) : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b
(<<) = concurrentOperator


-------------------CREATE MANY COMBINATIONS OF ARGUMENTS---------------------

public export
concat1 : ConcurrentWrap a -> ConcurrentWrap b -> (a -> b -> c) -> ConcurrentWrap c

public export
concat2 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> (a -> b -> c -> d) -> ConcurrentWrap d

public export
concat3 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> (a -> b -> c -> d -> e) -> ConcurrentWrap e

public export
concat4 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> (a -> b -> c -> d -> e -> f) -> ConcurrentWrap f

public export
concat5 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> (a -> b -> c -> d -> e -> f -> g) -> ConcurrentWrap g

public export
concat6 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> ConcurrentWrap g -> (a -> b -> c -> d -> e -> f -> g -> k) -> ConcurrentWrap k




