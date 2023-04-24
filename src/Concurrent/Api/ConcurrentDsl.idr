module Concurrent.Api.ConcurrentDsl

-----------------------------------------------------------------DSL----------------------------------------------------------------
public export
data ConcurrentWrap : Type -> Type where

export
concurrentOperator : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b

infixr 10 <<
export
(<<) : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b
(<<) = concurrentOperator


-------------------CREATE MANY COMBINATIONS OF ARGUMENTS---------------------

export
concat1 : ConcurrentWrap a -> ConcurrentWrap b -> (a -> b -> c) -> ConcurrentWrap c

export
concat2 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> (a -> b -> c -> d) -> ConcurrentWrap d

export
concat3 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> (a -> b -> c -> d -> e) -> ConcurrentWrap e

export
concat4 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> (a -> b -> c -> d -> e -> f) -> ConcurrentWrap f

export
concat5 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> (a -> b -> c -> d -> e -> f -> g) -> ConcurrentWrap g

export
concat6 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> ConcurrentWrap g -> (a -> b -> c -> d -> e -> f -> g -> k) -> ConcurrentWrap k




