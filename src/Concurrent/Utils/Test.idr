-- module Concurrent.Utils.Test

-- import Language.Reflection

-- %default total


-- data ArrowDo : (Type -> Type -> Type) -> Type -> Type

-- (>>=) : ArrowDo arr a -> (a -> ArrowDo arr b) -> ArrowDo arr b

-- (>>) : ArrowDo arr Unit -> ArrowDo arr b -> ArrowDo arr b

-- -- pure : Arrow arr => a -> ArrowDo arr a

-- infix 0 -<

-- (-<) : arr a b -> a -> ArrowDo arr b

-- -- A point where all parallel computations must be joined
-- fence : ArrowDo arr Unit

-- --- Usage examples ---

-- -- I guess, we'll need `arr`, `a` and `b` be runtime arguments of macro-functions.
-- -- Q: How to require ArrowChoice when needed?

-- %language ElabReflection

-- arrDoImpl : Arrow arr => TTImp -> Elab $ arr a b
-- arrDoImpl expr = do
--   logSugaredTerm "arrowdo" 0 "f" expr
--   _ <- check {expected = a -> Syntax.ArrowDo.ArrowDo arr b} expr
--   fail "Arrow-do syntax is not implemented yet"

-- %macro
-- arrDoQuoted : Arrow arr => TTImp -> Elab $ arr a b
-- arrDoQuoted = arrDoImpl

-- --failing "not implemented yet"
-- --
-- --  y : Arrow arr => arr Nat String -> arr String Bool -> arr Nat Bool
-- --  y ns sb = arrDoQuoted {arr} `( \n => do
-- --    s <- ns -< n + 1
-- --    b <- sb -< s
-- --    fence
-- --    pure b
-- --  )

-- %macro
-- arrDoDirect : Arrow arr => (a -> ArrowDo arr b) -> Elab $ arr a b
-- arrDoDirect f = arrDoImpl !(quote f)

-- failing "not implemented yet"

--   x : Arrow arr => arr Nat String -> arr String Bool -> arr Nat Bool
--   x ns sb = arrDoDirect $ \n => do
--     s <- ns -< n + 1
--     b <- sb -< s
--     fence
--     pure b

-- xx : Arrow arr => (a -> ArrowDo arr b) -> Elab Unit
-- xx f = ignore $ arrDoImpl {arr} {a} {b} !(quote f)

-- data X : Type -> Type -> Type where [external]

-- Category X where
--   (.) = ?comp_x
--   id = ?id_x

-- Arrow X where
--   first = ?first_x
--   arrow = ?arrow_x

-- ns : X Nat String
-- ns = ?ns_impl

-- sb : X String Bool
-- sb = ?sb_impl

-- failing "not implemented yet"

--   %runElab xx $ \n => do
--       s <- ns -< n + 1
--       b <- sb -< s
--       fence
--       pure b