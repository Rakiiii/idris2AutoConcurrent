module Concurrent.Api.ConcurrentDsl

public export
runConcurrent : IO a -> a
runConcurrent = unsafePerformIO
-----------------------------------------------------------------DSL----------------------------------------------------------------
public export
data ConcurrentWrap : Type -> Type where

public export
concurrentOperator : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b
concurrentOperator f x = ?concurrentOperator_rhs

infixr 10 <<
public export
(<<) : (a -> b) -> ConcurrentWrap a -> ConcurrentWrap b
(<<) = concurrentOperator


-------------------CREATE MANY COMBINATIONS OF ARGUMENTS---------------------

public export
map2 : ConcurrentWrap a -> ConcurrentWrap b -> (a -> b -> c) -> ConcurrentWrap c
map2 x y f = ?concat1_rhs

export
map3 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> (a -> b -> c -> d) -> ConcurrentWrap d

export
map4 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> (a -> b -> c -> d -> e) -> ConcurrentWrap e

export
map5 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> (a -> b -> c -> d -> e -> f) -> ConcurrentWrap f

export
map6 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> (a -> b -> c -> d -> e -> f -> g) -> ConcurrentWrap g

export
map7 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> ConcurrentWrap g -> (a -> b -> c -> d -> e -> f -> g -> k) -> ConcurrentWrap k

export
map8 : ConcurrentWrap a -> ConcurrentWrap b -> ConcurrentWrap c -> ConcurrentWrap d -> ConcurrentWrap e -> ConcurrentWrap f -> ConcurrentWrap g -> ConcurrentWrap k -> (a -> b -> c -> d -> e -> f -> g -> k -> h) -> ConcurrentWrap h


