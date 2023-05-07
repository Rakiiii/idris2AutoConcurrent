module Main

import public Concurrent.Api.ConcurrentDsl

import public Concurrent.Parser.LetParser.Utils
import public Concurrent.Parser.LetParser.Api

import public Concurrent.Types.Functions
import public Concurrent.Types.Execution
import public Concurrent.Types.DataFlowGraph

import public Concurrent.Parser.GraphConstructor

import public Concurrent.Partition.GraphPartitioner

import public Concurrent.Generator.FunctionGenerator

import public Concurrent.Scheduler.ConcurrentFunctionScheduler

import public Concurrent.Typer.FunctionTyper

import public  Concurrent.Utils.IR

import public Data.List
import public Data.Zippable
import public Control.Function

import public Language.Reflection
import public Language.Reflection.Derive
import public Language.Reflection.Types
import public Language.Reflection.Pretty
import public Language.Reflection.Syntax
import public Text.PrettyPrint.Bernardy

import public System
import public System.Concurrency
import public System.Future

import public Prelude

import public Concurrent.Api.Scripts


%default covering
%language ElabReflection


fib : Nat -> Nat
fib 0 = 0
fib 1 = 1
fib (S (S n)) = if n > 28 then fib 28 else fib (S n) + fib n

fib0 = fib
fib1 = fib
fib2 = fib
fib3 = fib
fib4 = fib
fib5 = fib
fib6 = fib
fib7 = fib
fib8 = fib
fib9 = fib
fib10 = fib
fib11 = fib
fib12 = fib
fib13 = fib
fib14 = fib
fib15 = fib
fib16 = fib
fib17 = fib
fib18 = fib
fib19 = fib
fib20 = fib
fib21 = fib
fib22 = fib
fib23 = fib
fib24 = fib
fib25 = fib
fib26 = fib
fib27 = fib
fib28 = fib

-- t1 = %runElab makeFunctionConcurrentOriginalPartition (KerniganLinParitioner 10 5) "conFib" Nat (Nat, Nat, Nat) $ \saw =>
--     let result1 = fib << saw
--     in let result2 = fib << concat1 saw result1 (+)
--     in let result3 = fib << result2
--     in id << concat2 result1 result2 result3 tuple

-- t2 = %runElab makeFunctionConcurrentSimplePartition (KerniganLinParitioner 10 5) "conFib" Nat (Nat, Nat, Nat) $ \saw =>
--     let result1 = fib << saw
--     in let result2 = fib << concat1 saw result1 (+)
--     in let result3 = fib << result2
--     in id << concat2 result1 result2 result3 tuple

-- %runElab makeFunctionConcurrent (KerniganLinParitioner 10 5) "conFib" Nat (Nat, Nat, Nat) $ \saw =>
--     let result1 = fib0 << saw
--     in let result2 = fib1 << concat1 saw result1 (+)
--     in let result3 = fib2 << result2
--     in id << concat2 result1 result2 result3 tuple

-- t = %runElab makeFunctionConcurrent' (KerniganLinParitioner 10 5) "conFib" Nat (Nat, Nat, Nat) $ \saw =>
--     let result1 = fib0 << saw
--     in let result2 = fib1 << concat1 saw result1 (+)
--     in let result3 = fib2 << result2
--     in id << concat2 result1 result2 result3 tuple

printIO : Show a => IO a -> IO ()
printIO ioX = do
    x <- ioX
    printLn $ show x














plus2 : Nat -> Nat -> Nat
plus2 = (+)

plus3 : Nat -> Nat -> Nat -> Nat
plus3 a1 a2 a3 = a1 + a2 + a3

plus4 : Nat -> Nat -> Nat -> Nat ->  Nat
plus4 a1 a2 a3 a4 = a1 + a2 + a3 + a4

plus5 : Nat -> Nat -> Nat -> Nat ->  Nat -> Nat
plus5 a1 a2 a3 a4 a5 = a1 + a2 + a3 + a4 + a5

plus8 : Nat -> Nat -> Nat -> Nat ->  Nat -> Nat -> Nat -> Nat -> Nat
plus8 a1 a2 a3 a4 a5 a6 a7 a8 = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8


-- %runElab makeFunctionConcurrent (KerniganLinParitioner 0 0) "fibBenchConc" Nat Nat $ \saw =>
--                     let v0 = fib0 << concat1 saw saw (+)

--                     in let v9 = fib1 << saw

--                     in let v7 = fib2 << saw

--                     in let v32 = fib3 << concat1 saw saw (*)

--                     in let v21 = fib4 << concat1 v7 saw (+)

--                     in let v34 = fib5 << concat2 v0 v21 v32 plus2

--                     in let v37 = fib6 << v34

--                     in let v30 = fib7 << concat1 saw v37 (+)

--                     in let v27 = fib8 << v21

--                     in let v8 = fib9 << concat1 v0 saw (+)

--                     in let v6 = fib10 << concat1 v8 v27 (+)

--                     in let v25 = fib11 << concat2 v6 saw saw plus2

--                     in let v39 = fib12 << saw

--                     in let v13 = fib13 << concat1 v25 v9 (+)

--                     in let v22 = fib14 << concat1 v13 v39 (+)

--                     in let v1 = fib15 << v22

--                     in let v3 = fib16 << concat1 v1 saw (+)

--                     in let v36 = fib17  << concat2 v25 saw saw plus2

--                     in let v23 = fib18  << concat1 v3 saw (+)

--                     in let v2 = fib19 << v23

--                     in let v14 = fib20 << v2

--                     in let v38 = fib21 << v3

--                     in let v28 = fib22 << v14

--                     in let v18 = fib23 << v28

--                     in let v24 = fib24 << v18

--                     in let v19 = fib25 << concat1 saw v18 (+)

--                     in let v26 = fib26 << concat1 v13 saw (+)

--                     in let v20 = fib27 << v26

--                     in let v35 = fib28 << v26

--                     in id << concat7 v19 v20 v24 v30 v34 v35 v36 v38 plus8


-- fibBenchOriginSmall : Nat -> Nat
-- fibBenchOriginSmall saw = do
--   let v0 = fib saw saw
--   let v1 = fib v0
--   let v2 = fib v1 saw
--   let v9 = fib v2
--   let v10 = fib v9

--   let v7 = fib v2
--   let v12 = fib v7
--   let v11 = fib v7

--   let v19 = fib saw
--   let v14 = fib v1 v19

--   let v15 = fib saw
--   let v13 = fib v15
--   let v17 = fib v12 v13 v15 saw v19
--   let v3 = fib v17
--   let v16 = fib v3

--   fib v3 v10 v11 v14 v16

-- fibBenchVerySmall : Nat -> Nat
-- fibBenchVerySmall saw = do
--   let v9 = fib saw
--   let v0 = fib saw
--   let v2 = fib v7
--   let v7 = fib v5 v9
--   let v1 = fib v0 v2 saw saw
--   v1

-- %runElab makeFunctionConcurrent (KerniganLinParitioner 1 10) "fibBenchConc" Nat Nat $ \saw =>
--   let v9 = fib0 << saw
--   in let v0 = fib1 << saw
--   in let v7 = fib3 << concat1 saw v9 (+)
--   in let v2 = fib2 << v7
--   in fib4 << concat3 v0 v2 saw saw plus4

fibBenchVerySmall' : ConcurrentWrap Nat -> ConcurrentWrap Nat
fibBenchVerySmall' saw =
  let v9 = fib0 << saw
  in let v0 = fib1 << saw
  in let v7 = fib3 << concat1 saw v9 (+)
  in let v2 = fib2 << v7
  in fib4 << concat3 v0 v2 saw saw plus4

-- %runElab makeFunctionConcurrent (KerniganLinParitioner 5 20) "fibBenchConc" Nat Nat $ \saw =>
--   let v0 = fib0 << concat1 saw saw (+)
--   in let v1 = fib1 << v0
--   in let v2 = fib2 << concat1 v1 saw (+)
--   in let v9 = fib3 << v2
--   in let v10 = fib4 << v9

--   in let v7 = fib5 << v2
--   in let v12 = fib6 << v7
--   in let v11 = fib7 << v7

--   in let v19 = fib8 << saw
--   in let v14 = fib9 << concat1 v1 v19 (+)

--   in let v15 = fib10 << saw
--   in let v13 = fib11 << v15
--   in let v17 = fib12 << concat4 v12 v13 v15 saw v19 plus5
--   in let v3 = fib13 << v17
--   in let v16 = fib14 << v3

--   in id << concat4 v3 v10 v11 v14 v16 plus5

-- %runElab makeFunctionConcurrent (KerniganLinParitioner 5 10) "fibBenchConc8" Nat Nat $ \saw =>
--   let v10 = fib10 << saw 
--   in let v5 = fib5 << concat1 v10 saw plus2
--   in let v13 = fib13 << v5 


--   in let v6 = fib6 << concat1 saw saw plus2 
--   in let v7 = fib7 << concat1 saw v6 plus2
--   in let v2 = fib2 << concat3 saw v6 saw saw plus4
--   in let v9 = fib9 <<  v7
--   in fib4 << concat2 v13 v9 v2 plus3

-- t8 = %runElab makeFunctionConcurrentOriginalPartition (KerniganLinParitioner 20 10) "fibBenchConc8" Nat Nat $ \saw =>
--   let v10 = fib10 << saw 
--   in let v5 = fib5 << concat1 v10 saw plus2
--   in let v13 = fib13 << v5 


--   in let v6 = fib6 << concat1 saw saw plus2 
--   in let v7 = fib7 << concat1 saw v6 plus2
--   in let v2 = fib2 << concat3 saw v6 saw saw plus4
--   in let v9 = fib9 <<  v7
--   in fib4 << concat2 v13 v9 v2 plus3
-- %runElab makeFunctionConcurrent (KerniganLinParitioner 5 10) "fibBenchConc6" Nat Nat $ \saw =>
--   let v10 = fib10 << saw 
--   in let v5 = fib5 << concat1 v10 saw plus2
--   in let v13 = fib13 << v5 


--   in let v6 = fib6 << concat1 saw saw plus2 
--   in let v7 = fib7 << concat1 saw v6 plus2
--   in fib4 << concat1 v13 v7 plus2

tmpBench6 : ConcurrentWrap Nat -> ConcurrentWrap Nat
tmpBench6 saw =
  let v10 = fib10 << saw 
  in let v5 = fib5 << concat1 v10 saw plus2
  in let v13 = fib13 << v5 


  in let v6 = fib6 << concat1 saw saw plus2 
  in let v7 = fib7 << concat1 saw v6 plus2
  in fib4 << concat1 v13 v7 plus2

-- %runElab makeFunctionConcurrent (KerniganLinParitioner 5 10) "fibBenchConc7" Nat Nat $ \saw =>
--   let v10 = fib10 << saw 
--   in let v5 = fib5 << concat1 v10 saw plus2
--   in let v13 = fib13 << v5 


--   in let v6 = fib6 << concat1 saw saw plus2 
--   in let v7 = fib7 << concat1 saw v6 plus2
--   in let v9 = fib9 <<  v7
--   in fib4 << concat1 v13 v9 plus2

tmpBench7 : ConcurrentWrap Nat -> ConcurrentWrap Nat
tmpBench7 saw =
  let v10 = fib10 << saw 
  in let v5 = fib5 << concat1 v10 saw plus2
  in let v13 = fib13 << v5 


  in let v6 = fib6 << concat1 saw saw plus2 
  in let v7 = fib7 << concat1 saw v6 plus2
  in let v9 = fib9 <<  v7
  in fib4 << concat1 v13 v9 plus2

%runElab makeFunctionConcurrent (KerniganLinParitioner 5 10) "fibBenchConc8" Nat Nat $ \saw =>
  let v10 = fib10 << saw 
  in let v5 = fib5 << concat1 v10 saw plus2
  in let v13 = fib13 << v5 


  in let v6 = fib6 << concat1 saw saw plus2 
  in let v7 = fib7 << concat1 saw v6 plus2
  in let v2 = fib2 << concat3 saw v6 saw saw plus4
  in let v9 = fib9 <<  v7
  in fib4 << concat2 v13 v9 v2 plus3

tmpBench8 : ConcurrentWrap Nat -> ConcurrentWrap Nat
tmpBench8 saw =
  let v10 = fib10 << saw 
  in let v5 = fib5 << concat1 v10 saw plus2
  in let v13 = fib13 << v5 


  in let v6 = fib6 << concat1 saw saw plus2 
  in let v7 = fib7 << concat1 saw v6 plus2
  in let v2 = fib2 << concat3 saw v6 saw saw plus4
  in let v9 = fib9 <<  v7
  in fib4 << concat2 v13 v9 v2 plus3

-- %runElab makeFunctionConcurrent (KerniganLinParitioner 5 15) "fibBenchConc11" Nat Nat $ \saw =>
--   let v11 = fib11 <<  saw 
--   in let v2 = fib2 <<  v11 
--   in let v0 = fib0 <<  v2 
--   in let v4 = fib4 <<  v0 

--   in let v13 = fib13 << concat2 v4 saw saw plus3
--   in let v12 = fib12 <<  v13 
--   in let v14 = fib14 <<  v12 


--   in let v5 = fib5 <<  saw 
--   in let v1 = fib1 << concat1 v5 saw plus2
--   in let v10 = fib10 <<  v1 

--   in id <<  concat2 v14 v10 v4 plus3

tmpBench11 : ConcurrentWrap Nat -> ConcurrentWrap Nat
tmpBench11 saw = 
  let v11 = fib11 <<  saw 
  in let v2 = fib2 <<  v11 
  in let v0 = fib0 <<  v2 
  in let v4 = fib4 <<  v0 

  in let v13 = fib13 << concat2 v4 saw saw plus3
  in let v12 = fib12 <<  v13 
  in let v14 = fib14 <<  v12 


  in let v5 = fib5 <<  saw 
  in let v1 = fib1 << concat1 v5 saw plus2
  in let v10 = fib10 <<  v1 

  in id <<  concat2 v14 v10 v4 plus3

fibBenchSmall : ConcurrentWrap Nat -> ConcurrentWrap Nat
fibBenchSmall saw =
  let v0 = fib << concat1 saw saw (+)
  in let v1 = fib << v0
  in let v2 = fib << concat1 v1 saw (+)
  in let v9 = fib << v2
  in let v10 = fib << v9

  in let v7 = fib << v2
  in let v12 = fib << v7
  in let v11 = fib << v7

  in let v19 = fib << saw
  in let v14 = fib << concat1 v1 v19 (+)

  in let v15 = fib << saw
  in let v13 = fib << v15
  in let v17 = fib << concat4 v12 v13 v15 saw v19 plus5
  in let v3 = fib << v17
  in let v16 = fib << v3

  in id << concat4 v3 v10 v11 v14 v16 plus5

fibBenchOrigin : Nat -> Nat
fibBenchOrigin saw = do
                    let v0 = fib0 $ saw + saw

                    let v9 = fib1 saw

                    let v7 = fib2 saw

                    let v32 = fib3 $ saw * saw

                    let v21 = fib4 $ v7 + saw

                    let v34 = fib5 $ v0 + v21 + v32

                    let v37 = fib6 v34

                    let v30 = fib7 $ saw + v37

                    let v27 = fib8 v21

                    let v8 = fib9 $ v0 + saw

                    let v6 = fib10 $ v8 + v27

                    let v25 = fib11 $ v6 + saw + saw

                    let v39 = fib12 saw

                    let v13 = fib13 $ v25 + v9

                    let v22 = fib14 $ v13 + v39

                    let v1 = fib15 v22

                    let v3 = fib16 $ v1 + saw

                    let v36 = fib17 $ v25 + saw + saw

                    let v23 = fib18 $ v3 + saw

                    let v2 = fib19 v23

                    let v14 = fib20 v2

                    let v38 = fib21 v3

                    let v28 = fib22 v14

                    let v18 = fib23 v28

                    let v24 = fib24 v18

                    let v19 = fib25 $ saw + v18

                    let v26 = fib26 $ v13 + saw

                    let v20 = fib27 v26

                    let v35 = fib28 v26

                    let v15 = fib $ v19 + v20 + v24 + v30 + v34 + v35 + v36 + v38
                    v15

fibBenchRef : ConcurrentWrap Nat -> ConcurrentWrap Nat
fibBenchRef saw =
                    let v0 = fib0 << concat1 saw saw (+)

                    in let v9 = fib1 << saw

                    in let v7 = fib2 << saw

                    in let v32 = fib3 << concat1 saw saw (*)

                    in let v21 = fib4 << concat1 v7 saw (+)

                    in let v34 = fib5 << concat2 v0 v21 v32 plus3

                    in let v37 = fib6 << v34

                    in let v30 = fib7 << concat1 saw v37 (+)

                    in let v27 = fib8 << v21

                    in let v8 = fib9 << concat1 v0 saw (+)

                    in let v6 = fib10 << concat1 v8 v27 (+)

                    in let v25 = fib11 << concat2 v6 saw saw plus3

                    in let v39 = fib12 << saw

                    in let v13 = fib13 << concat1 v25 v9 (+)

                    in let v22 = fib14 << concat1 v13 v39 (+)

                    in let v1 = fib15 << v22

                    in let v3 = fib16 << concat1 v1 saw (+)

                    in let v36 = fib17  << concat2 v25 saw saw plus3

                    in let v23 = fib18  << concat1 v3 saw (+)

                    in let v2 = fib19 << v23

                    in let v14 = fib20 << v2

                    in let v38 = fib21 << v3

                    in let v28 = fib22 << v14

                    in let v18 = fib23 << v28

                    in let v24 = fib24 << v18

                    in let v19 = fib25 << concat1 saw v18 (+)

                    in let v26 = fib26 << concat1 v13 saw (+)

                    in let v20 = fib27 << v26

                    in let v35 = fib28 << v26

                    in id << concat7 v19 v20 v24 v30 v34 v35 v36 v38 plus8
