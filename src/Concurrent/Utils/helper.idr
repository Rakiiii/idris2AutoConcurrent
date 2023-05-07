






-- Bad elaborator script foldr ((*>) . declare) (pure ()) (fst (either (Delay (\_ => ([], []))) (Delay id) (rxJoin (rxMap (rxMapInternalFst (rxMapInternalFst (rxMapInternalFst
--  (rxMapInternalFst (parseLambdaFunction (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 27) (252, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 32) (70, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 41) (250, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 32) (64, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 64) (250, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 54) (252, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) ?delayed) 
-- constructDataDependencieGraph) (dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1))) (\lamc => let (graph, partition) = lamc in let functionsBodies = mapT generateFunctionBody partition in (graph, (partition, functionsBodies)))) (\lamc => let ((graph, (partition, bodies)), argType) = lamc in composeFunctions (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 27) (252, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 32) (70, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 41) (250, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 32) (64, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 64) (250, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 54) (252, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments"))))))) (fromString "concurrentFunctionBaseName") partition graph bodies)) ()))) (script is not a data value).






-- Bad elaborator script foldr ((*>) . declare) (pure ()) (fst (either (Delay (\_ => ([], []))) (Delay id) (rxJoin (rxMap (rxMapInternalFst (rxMapInternalFst (rxMapInternalFst (rxMapInternalFst (parseLambdaFunction (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 27) (253, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 32) (71, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 21) (71, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 41) (252, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 21) (68, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 32) (65, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 21) (65, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 64) (251, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 54) (253, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) ?delayed) constructDataDependencieGraph) (dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1))) (\lamc => let (graph, partition) = lamc in let functionsBodies = mapT generateFunctionBody partition in (graph, (partition, functionsBodies)))) (\lamc => let ((graph, (partition, bodies)), argType) = lamc in composeFunctions (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 27) (253, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 32) (71, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 21) (71, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 41) (252, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 21) (68, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 32) (65, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 21) (65, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 64) (251, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 54) (253, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments"))))))) (fromString "concurrentFunctionBaseName") partition graph bodies)) ()))) (script is not a data value).
-- idris2
( [ [ IClaim
        emptyFC
        MW
        Public
        []
        (mkTy
           { name = "testMakeFunctionConcurrentBI_concurrent_function_1"
           , type =
                   MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
               .-> var "IO" .$ var "Unit"
           })
    , IDef
        emptyFC
        "testMakeFunctionConcurrentBI_concurrent_function_1"
        [       var "testMakeFunctionConcurrentBI_concurrent_function_1"
             .$ bindVar
                  "channel_Main_concurrentFunction2_saw_Main_concurrentFunction1_result_0_"
             .$ bindVar "channel_Main_concurrentFunction3_Main_concurrentFunction2_result_0_"
             .$ bindVar "channel_Main_concurrentFunction1_saw_"
             .$ bindVar
                  "channel_Main_concurrentFunction4_Main_concurrentFunction1_result_0_Main_concurrentFunction3_result_0_"
             .$ bindVar "saw"
          .=    var "Prelude.Interfaces.(>>=)"
             .$ (   var "System.Concurrency.channelGet"
                 .$ var "channel_Main_concurrentFunction1")
             .$ (    MkArg
                       MW
                       ExplicitArg
                       (Just "Main_concurrentFunction1_result_0")
                       implicitFalse
                 .=> ilet
                       { count = MW
                       , name = "Main_concurrentFunction2_result_0"
                       , type = implicitTrue
                       , val =
                              var "Main.concurrentFunction2"
                           .$ (   var "Main.concatArguments"
                               .$ var "saw"
                               .$ var "Main_concurrentFunction1_result_0")
                       , scope =
                              var "Prelude.Interfaces.(>>)"
                           .$ (   var "System.Concurrency.channelPut"
                               .$ var "channel_Main_concurrentFunction2"
                               .$ var "Main_concurrentFunction2_result_0")
                           .$ (   var "System.Concurrency.channelPut"
                               .$ var "channel_Main_concurrentFunction3"
                               .$ (   var "Main.concurrentFunction3"
                                   .$ var "Main_concurrentFunction2_result_0"))
                       })
        ]
    ]
  , [ IClaim
        emptyFC
        MW
        Public
        []
        (mkTy
           { name = "testMakeFunctionConcurrentBI_concurrent_function_2"
           , type =
                   MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ primVal (PrT IntegerType))
               .-> MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
               .-> var "IO" .$ var "Unit"
           })
    , IDef
        emptyFC
        "testMakeFunctionConcurrentBI_concurrent_function_2"
        [       var "testMakeFunctionConcurrentBI_concurrent_function_2"
             .$ bindVar
                  "channel_Main_concurrentFunction2_saw_Main_concurrentFunction1_result_0_"
             .$ bindVar "channel_Main_concurrentFunction3_Main_concurrentFunction2_result_0_"
             .$ bindVar "channel_Main_concurrentFunction1_saw_"
             .$ bindVar
                  "channel_Main_concurrentFunction4_Main_concurrentFunction1_result_0_Main_concurrentFunction3_result_0_"
             .$ bindVar "saw"
          .= ilet
               { count = MW
               , name = "Main_concurrentFunction1_result_0"
               , type = implicitTrue
               , val = var "Main.concurrentFunction1" .$ var "saw"
               , scope =
                      var "Prelude.Interfaces.(>>)"
                   .$ (   var "System.Concurrency.channelPut"
                       .$ var "channel_Main_concurrentFunction1"
                       .$ var "Main_concurrentFunction1_result_0")
                   .$ (   var "Prelude.Interfaces.(>>=)"
                       .$ (   var "System.Concurrency.channelGet"
                           .$ var "channel_Main_concurrentFunction3")
                       .$ (    MkArg
                                 MW
                                 ExplicitArg
                                 (Just "Main_concurrentFunction3_result_0")
                                 implicitFalse
                           .=>    var "System.Concurrency.channelPut"
                               .$ var "channel_Main_concurrentFunction4"
                               .$ (   var "Main.concurrentFunction4"
                                   .$ (   var "Main.concatArguments"
                                       .$ var "Main_concurrentFunction1_result_0"
                                       .$ var "Main_concurrentFunction3_result_0"))))
               }
        ]
    ]
  ]
, [ IClaim
      emptyFC
      MW
      Public
      []
      (mkTy
         { name = "testMakeFunctionConcurrentBI"
         , type =
                 MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
             .-> var "IO" .$ primVal (PrT IntegerType)
         })
  , IDef
      emptyFC
      "testMakeFunctionConcurrentBI"
      [    var "testMakeFunctionConcurrentBI" .$ bindVar "saw"
        .=    var "Prelude.Interfaces.(>>=)"
           .$ (   var "System.Concurrency.makeChannel"
               .! ("a", primVal (PrT IntegerType))
               .! ("io", var "PrimIO.IO"))
           .$ (    MkArg MW ExplicitArg (Just "channel_Main_concurrentFunction2") implicitFalse
               .=>    var "Prelude.Interfaces.(>>=)"
                   .$ (   var "System.Concurrency.makeChannel"
                       .! ("a", primVal (PrT IntegerType))
                       .! ("io", var "PrimIO.IO"))
                   .$ (    MkArg MW ExplicitArg (Just "channel_Main_concurrentFunction3") implicitFalse
                       .=>    var "Prelude.Interfaces.(>>=)"
                           .$ (   var "System.Concurrency.makeChannel"
                               .! ("a", primVal (PrT IntegerType))
                               .! ("io", var "PrimIO.IO"))
                           .$ (    MkArg MW ExplicitArg (Just "channel_Main_concurrentFunction1") implicitFalse
                               .=>    var "Prelude.Interfaces.(>>=)"
                                   .$ (   var "System.Concurrency.makeChannel"
                                       .! ("a", primVal (PrT IntegerType))
                                       .! ("io", var "PrimIO.IO"))
                                   .$ (    MkArg MW ExplicitArg (Just "channel_Main_concurrentFunction4") implicitFalse
                                       .=>    var "Prelude.Interfaces.(>>=)"
                                           .$ (   var "Prelude.IO.fork"
                                               .$ (   var "testMakeFunctionConcurrentBI_concurrent_function_1"
                                                   .$ var "channel_Main_concurrentFunction2"
                                                   .$ var "channel_Main_concurrentFunction3"
                                                   .$ var "channel_Main_concurrentFunction1"
                                                   .$ var "channel_Main_concurrentFunction4"
                                                   .$ var "saw"))
                                           .$ (    MkArg MW ExplicitArg Nothing (var "PrimIO.ThreadID")
                                               .=>    var "Prelude.Interfaces.(>>=)"
                                                   .$ (   var "Prelude.IO.fork"
                                                       .$ (   var
                                                                "testMakeFunctionConcurrentBI_concurrent_function_2"
                                                           .$ var "channel_Main_concurrentFunction2"
                                                           .$ var "channel_Main_concurrentFunction3"
                                                           .$ var "channel_Main_concurrentFunction1"
                                                           .$ var "channel_Main_concurrentFunction4"
                                                           .$ var "saw"))
                                                   .$ (    MkArg MW ExplicitArg Nothing (var "PrimIO.ThreadID")
                                                       .=>    var "System.Concurrency.channelGet"
                                                           .! ("a", primVal (PrT IntegerType))
                                                           .! ("io", var "PrimIO.IO")
                                                           .$ var "channel_Main_concurrentFunction4"))))))
      ]
  ]
)






graph:[
    node index: 0 weight: 1 connected nodes: [2], 
    node index: 1 weight: 1 connected nodes: [0], 
    node index: 2 weight: 1 connected nodes: [], 
    node index: 3 weight: 1 connected nodes: [1, 2]
    ]



graph:[
    node index: 0 weight: 1 part: Right external ribs: 0 internal ribs: 1 connected nodes: [2], 
    node index: 1 weight: 1 part: Right external ribs: 0 internal ribs: 1 connected nodes: [0], 
    node index: 2 weight: 1 part: Right external ribs: 0 internal ribs: 0 connected nodes: [], 
    node index: 3 weight: 1 part: Right external ribs: 0 internal ribs: 2 connected nodes: [1, 2]
    ]








[ 
Function: "Main.concurrentFunction2" 
    Called with concatanation of: ["saw", "Main_concurrentFunction1_result_0"] 
        concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: "Main.concatArguments"; 
            Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction2_result_0"]
                Dependencies: ["Main_concurrentFunction1_result_0"], 
  
Function: "Main.concurrentFunction3" 
    Called with wraped variable: "Main_concurrentFunction2_result_0"; 
        Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction3_result_0"]
            Dependencies: ["Main_concurrentFunction2_result_0"], 
  
Function: "Main.concurrentFunction1" 
    Called with wraped variable: "saw"; 
        Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction1_result_0"]
            Dependencies: [], 
  
Function: "Main.concurrentFunction4" 
    Called with concatanation of: ["Main_concurrentFunction1_result_0", "Main_concurrentFunction3_result_0"] 
        concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: "Main.concatArguments"; 
            Returns type IntegerType Result of type IntegerType was not saved
                Dependencies: ["Main_concurrentFunction3_result_0", "Main_concurrentFunction1_result_0"]
]

([[IClaim EmptyFC MW Public [] (MkTy EmptyFC EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_1")) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                            "Generator",
                                                                                                                                                                                                            "Concurrent"])) (44,
   17) (44, 43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (44, 17) (44,
   43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (44, 17) (44,
   43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (44, 17) (44,
   43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (73, 21) (73,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (241, 89) (241,
   96)) (PrT IntegerType)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 106) (224, 111)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                          "Generator",
                                                                                                                                                                          "Concurrent"])) (224, 106) (224,
   108)) (UN (Basic "IO"))) (IAlternative (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224,
   111)) (UniqueDefault (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224,
   111)) (UN (Basic "MkUnit")))) [IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224, 111)) (UN (Basic "Unit")),
                                  IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224, 111)) (UN (Basic "MkUnit"))])))))))),
   IDef EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_1")) [PatClause EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IVar EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_1"))) (IBindVar EmptyFC "channel_Main_concurrentFunction2")) (IBindVar EmptyFC "channel_Main_concurrentFunction3")) (IBindVar EmptyFC "channel_Main_concurrentFunction1")) (IBindVar EmptyFC "channel_Main_concurrentFunction4")) (IBindVar EmptyFC "saw")) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "Generator",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "Concurrent"])) (41,
                                                                                   27) (41, 51)) (NS (MkNS ["Interfaces",
                                                                                                            "Prelude"]) (UN (Basic ">>=")))) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                              "Generator",
                                                                                                                                                                                              "Concurrent"])) (46,
                                                                                   17) (46, 46)) (NS (MkNS ["Concurrency",
                                                                                                            "System"]) (UN (Basic "channelGet")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction1"))))) (ILam EmptyFC MW ExplicitArg (Just (UN (Basic "Main_concurrentFunction1_result_0"))) (Implicit EmptyFC False) (ILet EmptyFC EmptyFC MW (UN (Basic "Main_concurrentFunction2_result_0")) (Implicit EmptyFC True) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (243,
                                                                                   21) (243,
                                                                                   40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2")))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (243,
                                                                                   64) (243, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (241,
                                                                                   108) (241,
                                                                                   111)) (UN (Basic "saw")))) (IVar EmptyFC (UN (Basic "Main_concurrentFunction1_result_0"))))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                               "Generator",
                                                                                                                                                                                                                                               "Concurrent"])) (40,
                                                                                   27) (40, 50)) (NS (MkNS ["Interfaces",
                                                                                                            "Prelude"]) (UN (Basic ">>")))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                           "Generator",
                                                                                                                                                                                                           "Concurrent"])) (45,
                                                                                   17) (45, 46)) (NS (MkNS ["Concurrency",
                                                                                                            "System"]) (UN (Basic "channelPut")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction2")))) (IVar EmptyFC (UN (Basic "Main_concurrentFunction2_result_0"))))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                                    "Generator",
                                                                                                                                                                                                                                                                                                                                                    "Concurrent"])) (45,
                                                                                   17) (45, 46)) (NS (MkNS ["Concurrency",
                                                                                                            "System"]) (UN (Basic "channelPut")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction3")))) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (244,
                                                                                   21) (244,
                                                                                   40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3")))) (IVar EmptyFC (UN (Basic "Main_concurrentFunction2_result_0")))))))))]],
  [IClaim EmptyFC MW Public [] (MkTy EmptyFC EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_2")) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                            "Generator",
                                                                                                                                                                                                            "Concurrent"])) (44,
   17) (44, 43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (44, 17) (44,
   43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (44, 17) (44,
   43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (44, 17) (44,
   43)) (NS (MkNS ["Concurrency", "System"]) (UN (Basic "Channel")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (73, 21) (73,
   28)) (PrT IntegerType))) (IPi EmptyFC MW ExplicitArg Nothing (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (241, 89) (241,
   96)) (PrT IntegerType)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 106) (224, 111)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                          "Generator",
                                                                                                                                                                          "Concurrent"])) (224, 106) (224,
   108)) (UN (Basic "IO"))) (IAlternative (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224,
   111)) (UniqueDefault (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224,
   111)) (UN (Basic "MkUnit")))) [IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224, 111)) (UN (Basic "Unit")),
                                  IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (224, 109) (224, 111)) (UN (Basic "MkUnit"))])))))))),
   IDef EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_2")) [PatClause EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IVar EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_2"))) (IBindVar EmptyFC "channel_Main_concurrentFunction2")) (IBindVar EmptyFC "channel_Main_concurrentFunction3")) (IBindVar EmptyFC "channel_Main_concurrentFunction1")) (IBindVar EmptyFC "channel_Main_concurrentFunction4")) (IBindVar EmptyFC "saw")) (ILet EmptyFC EmptyFC MW (UN (Basic "Main_concurrentFunction1_result_0")) (Implicit EmptyFC True) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (242,
                                                                                   18) (242,
                                                                                   37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1")))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (241,
                                                                                   108) (241, 111)) (UN (Basic "saw")))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                        "Generator",
                                                                                                                                                                                        "Concurrent"])) (40,
                                                                                   27) (40, 50)) (NS (MkNS ["Interfaces",
                                                                                                            "Prelude"]) (UN (Basic ">>")))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                           "Generator",
                                                                                                                                                                                                           "Concurrent"])) (45,
                                                                                   17) (45, 46)) (NS (MkNS ["Concurrency",
                                                                                                            "System"]) (UN (Basic "channelPut")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction1")))) (IVar EmptyFC (UN (Basic "Main_concurrentFunction1_result_0"))))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                                    "Generator",
                                                                                                                                                                                                                                                                                                                                                    "Concurrent"])) (41,
                                                                                   27) (41, 51)) (NS (MkNS ["Interfaces",
                                                                                                            "Prelude"]) (UN (Basic ">>=")))) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                              "Generator",
                                                                                                                                                                                              "Concurrent"])) (46,
                                                                                   17) (46, 46)) (NS (MkNS ["Concurrency",
                                                                                                            "System"]) (UN (Basic "channelGet")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction3"))))) (ILam EmptyFC MW ExplicitArg (Just (UN (Basic "Main_concurrentFunction3_result_0"))) (Implicit EmptyFC False) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                                                                                 "Generator",
                                                                                                                                                                                                                                                                                                                                                                                                 "Concurrent"])) (45,
                                                                                   17) (45, 46)) (NS (MkNS ["Concurrency",
                                                                                                            "System"]) (UN (Basic "channelPut")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction4")))) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (245,
                                                                                   7) (245,
                                                                                   26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4")))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (245,
                                                                                   54) (245,
                                                                                   69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))) (IVar EmptyFC (UN (Basic "Main_concurrentFunction1_result_0")))) (IVar EmptyFC (UN (Basic "Main_concurrentFunction3_result_0"))))))))))]]],
[IClaim EmptyFC MW Public [] (MkTy EmptyFC EmptyFC (UN (Basic "testMakeFunctionConcurrentKL")) (IPi EmptyFC MW ExplicitArg Nothing (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (241, 89) (241,
 96)) (PrT IntegerType)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (373, 71) (373, 95)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                      "Generator",
                                                                                                                                                                      "Concurrent"])) (373, 71) (373,
 73)) (UN (Basic "IO"))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (241, 97) (241, 104)) (PrT IntegerType))))),
 IDef EmptyFC (UN (Basic "testMakeFunctionConcurrentKL")) [PatClause EmptyFC (IApp EmptyFC (IVar EmptyFC (UN (Basic "testMakeFunctionConcurrentKL"))) (IBindVar EmptyFC "saw")) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                               "Generator",
                                                                                                                                                                                                                                               "Concurrent"])) (41,
                                                           27) (41, 51)) (NS (MkNS ["Interfaces",
                                                                                    "Prelude"]) (UN (Basic ">>=")))) (INamedApp EmptyFC (INamedApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                              "Generator",
                                                                                                                                                                                              "Concurrent"])) (47,
                                                           17) (47, 47)) (NS (MkNS ["Concurrency",
                                                                                    "System"]) (UN (Basic "makeChannel")))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67,
                                                           28)) (PrT IntegerType))) (UN (Basic "io")) (IVar EmptyFC (NS (MkNS ["PrimIO"]) (UN (Basic "IO")))))) (ILam EmptyFC MW ExplicitArg (Just (UN (Basic "channel_Main_concurrentFunction2"))) (Implicit EmptyFC False) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                            "Generator",
                                                                                                                                                                                                                                                                                                                                            "Concurrent"])) (41,
                                                           27) (41, 51)) (NS (MkNS ["Interfaces",
                                                                                    "Prelude"]) (UN (Basic ">>=")))) (INamedApp EmptyFC (INamedApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                              "Generator",
                                                                                                                                                                                              "Concurrent"])) (47,
                                                           17) (47, 47)) (NS (MkNS ["Concurrency",
                                                                                    "System"]) (UN (Basic "makeChannel")))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70,
                                                           28)) (PrT IntegerType))) (UN (Basic "io")) (IVar EmptyFC (NS (MkNS ["PrimIO"]) (UN (Basic "IO")))))) (ILam EmptyFC MW ExplicitArg (Just (UN (Basic "channel_Main_concurrentFunction3"))) (Implicit EmptyFC False) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                            "Generator",
                                                                                                                                                                                                                                                                                                                                            "Concurrent"])) (41,
                                                           27) (41, 51)) (NS (MkNS ["Interfaces",
                                                                                    "Prelude"]) (UN (Basic ">>=")))) (INamedApp EmptyFC (INamedApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                              "Generator",
                                                                                                                                                                                              "Concurrent"])) (47,
                                                           17) (47, 47)) (NS (MkNS ["Concurrency",
                                                                                    "System"]) (UN (Basic "makeChannel")))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64,
                                                           28)) (PrT IntegerType))) (UN (Basic "io")) (IVar EmptyFC (NS (MkNS ["PrimIO"]) (UN (Basic "IO")))))) (ILam EmptyFC MW ExplicitArg (Just (UN (Basic "channel_Main_concurrentFunction1"))) (Implicit EmptyFC False) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                            "Generator",
                                                                                                                                                                                                                                                                                                                                            "Concurrent"])) (41,
                                                           27) (41, 51)) (NS (MkNS ["Interfaces",
                                                                                    "Prelude"]) (UN (Basic ">>=")))) (INamedApp EmptyFC (INamedApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                              "Generator",
                                                                                                                                                                                              "Concurrent"])) (47,
                                                           17) (47, 47)) (NS (MkNS ["Concurrency",
                                                                                    "System"]) (UN (Basic "makeChannel")))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (73, 21) (73,
                                                           28)) (PrT IntegerType))) (UN (Basic "io")) (IVar EmptyFC (NS (MkNS ["PrimIO"]) (UN (Basic "IO")))))) (ILam EmptyFC MW ExplicitArg (Just (UN (Basic "channel_Main_concurrentFunction4"))) (Implicit EmptyFC False) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                            "Generator",
                                                                                                                                                                                                                                                                                                                                            "Concurrent"])) (41,
                                                           27) (41, 51)) (NS (MkNS ["Interfaces", "Prelude"]) (UN (Basic ">>=")))) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                    "Generator",
                                                                                                                                                                                    "Concurrent"])) (49,
                                                           14) (49, 29)) (NS (MkNS ["IO",
                                                                                    "Prelude"]) (UN (Basic "fork")))) (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IVar EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_1"))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction2")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction3")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction1")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction4")))) (IVar EmptyFC (UN (Basic "saw")))))) (ILam EmptyFC MW ExplicitArg Nothing (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Generator",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Concurrent"])) (51,
                                                           14) (51, 29)) (NS (MkNS ["PrimIO"]) (UN (Basic "ThreadID")))) (IApp EmptyFC (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                        "Generator",
                                                                                                                                                                                        "Concurrent"])) (41,
                                                           27) (41, 51)) (NS (MkNS ["Interfaces", "Prelude"]) (UN (Basic ">>=")))) (IApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                    "Generator",
                                                                                                                                                                                    "Concurrent"])) (49,
                                                           14) (49, 29)) (NS (MkNS ["IO",
                                                                                    "Prelude"]) (UN (Basic "fork")))) (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IApp EmptyFC (IVar EmptyFC (UN (Basic "testMakeFunctionConcurrentKL_concurrent_function_2"))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction2")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction3")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction1")))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction4")))) (IVar EmptyFC (UN (Basic "saw")))))) (ILam EmptyFC MW ExplicitArg Nothing (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Generator",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Concurrent"])) (51,
                                                           14) (51,
                                                           29)) (NS (MkNS ["PrimIO"]) (UN (Basic "ThreadID")))) (IApp EmptyFC (INamedApp EmptyFC (INamedApp EmptyFC (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator",
                                                                                                                                                                                                       "Generator",
                                                                                                                                                                                                       "Concurrent"])) (46,
                                                           17) (46, 46)) (NS (MkNS ["Concurrency",
                                                                                    "System"]) (UN (Basic "channelGet")))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (73, 21) (73,
                                                           28)) (PrT IntegerType))) (UN (Basic "io")) (IVar (MkFC (PhysicalIdrSrc (MkMI ["FunctionGenerator", "Generator", "Concurrent"])) (50, 14) (50,
                                                           23)) (NS (MkNS ["PrimIO"]) (UN (Basic "IO"))))) (IVar EmptyFC (UN (Basic "channel_Main_concurrentFunction4"))))))))))))))))]])


















( [ [ IClaim
        emptyFC
        MW
        Public
        []
        (mkTy
           { name = "conFib_concurrent_function_1"
           , type =
                   MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ var "Prelude.Types.Nat")
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ var "Prelude.Types.Nat")
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ var "Prelude.Types.Nat")
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ (   var "Builtin.Pair"
                          .$ var "Prelude.Types.Nat"
                          .$ (   var "Builtin.Pair"
                              .$ var "Prelude.Types.Nat"
                              .$ var "Prelude.Types.Nat")))
               .-> MkArg MW ExplicitArg Nothing (var "Prelude.Types.Nat")
               .-> var "IO" .$ var "Unit"
           })
    , IDef
        emptyFC
        "conFib_concurrent_function_1"
        [       var "conFib_concurrent_function_1"
             .$ bindVar "channel_Main_fib_Main_fib_result_0_"
             .$ bindVar "channel_Main_fib_saw_Main_fib_result_0_"
             .$ bindVar "channel_Main_fib_saw_"
             .$ bindVar
                  "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_"
             .$ bindVar "saw"
          .=    var "Prelude.Interfaces.(>>=)"
             .$ (var "System.Concurrency.channelGet" .$ var "channel_Main_fib_saw_")
             .$ (    MkArg MW ExplicitArg (Just "Main_fib_result_0") implicitFalse
                 .=> ilet
                       { count = MW
                       , name = "Main_fib_result_0"
                       , type = implicitTrue
                       , val =
                              var "Main.fib"
                           .$ (   var "Prelude.Num.(+)"
                               .! ("ty", var "Prelude.Types.Nat")
                               .! ("__con", hole "__con")
                               .$ var "saw"
                               .$ var "Main_fib_result_0")
                       , scope =
                              var "Prelude.Interfaces.(>>)"
                           .$ (   var "System.Concurrency.channelPut"
                               .$ var "channel_Main_fib_saw_Main_fib_result_0_"
                               .$ var "Main_fib_result_0")
                           .$ (   var "System.Concurrency.channelPut"
                               .$ var "channel_Main_fib_Main_fib_result_0_"
                               .$ (var "Main.fib" .$ var "Main_fib_result_0"))
                       })
        ]
    ]
  , [ IClaim
        emptyFC
        MW
        Public
        []
        (mkTy
           { name = "conFib_concurrent_function_2"
           , type =
                   MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ var "Prelude.Types.Nat")
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ var "Prelude.Types.Nat")
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (var "System.Concurrency.Channel" .$ var "Prelude.Types.Nat")
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ (   var "Builtin.Pair"
                          .$ var "Prelude.Types.Nat"
                          .$ (   var "Builtin.Pair"
                              .$ var "Prelude.Types.Nat"
                              .$ var "Prelude.Types.Nat")))
               .-> MkArg MW ExplicitArg Nothing (var "Prelude.Types.Nat")
               .-> var "IO" .$ var "Unit"
           })
    , IDef
        emptyFC
        "conFib_concurrent_function_2"
        [       var "conFib_concurrent_function_2"
             .$ bindVar "channel_Main_fib_Main_fib_result_0_"
             .$ bindVar "channel_Main_fib_saw_Main_fib_result_0_"
             .$ bindVar "channel_Main_fib_saw_"
             .$ bindVar "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_"
             .$ bindVar "saw"
          .=    var "Prelude.Interfaces.(>>=)"
             .$ (   var "System.Concurrency.channelGet"
                 .$ var "channel_Main_fib_saw_Main_fib_result_0_")
             .$ (    MkArg MW ExplicitArg (Just "Main_fib_result_0") implicitFalse
                 .=>    var "Prelude.Interfaces.(>>=)"
                     .$ (   var "System.Concurrency.channelGet"
                         .$ var "channel_Main_fib_Main_fib_result_0_")
                     .$ (    MkArg MW ExplicitArg (Just "Main_fib_result_0") implicitFalse
                         .=>    var "System.Concurrency.channelPut"
                             .$ var
                                  "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_"
                             .$ (   var "Prelude.Basics.id"
                                 .! ( "a"
                                    ,    var "Builtin.Pair"
                                      .$ var "Prelude.Types.Nat"
                                      .$ (   var "Builtin.Pair"
                                          .$ var "Prelude.Types.Nat"
                                          .$ var "Prelude.Types.Nat")
                                    )
                                 .$ (   hole "arg"
                                     .$ var "Main_fib_result_0"
                                     .$ var "Main_fib_result_0"
                                     .$ var "Main_fib_result_0"))))
        ]
    ]
  ]
, [ IClaim
      emptyFC
      MW
      Public
      []
      (mkTy
         { name = "conFib"
         , type =
                 MkArg MW ExplicitArg Nothing (var "Prelude.Types.Nat")
             .->    var "IO"
                 .$ (   var "Builtin.Pair"
                     .$ var "Prelude.Types.Nat"
                     .$ (   var "Builtin.Pair"
                         .$ var "Prelude.Types.Nat"
                         .$ var "Prelude.Types.Nat"))
         })
  , IDef
      emptyFC
      "conFib"
      [    var "conFib" .$ bindVar "saw"
        .=    var "Prelude.Interfaces.(>>=)"
           .$ (   var "System.Concurrency.makeChannel"
               .! ("a", var "Prelude.Types.Nat")
               .! ("io", var "PrimIO.IO"))
           .$ (    MkArg
                     MW
                     ExplicitArg
                     (Just "channel_Main_fib_Main_fib_result_0_")
                     implicitFalse
               .=>    var "Prelude.Interfaces.(>>=)"
                   .$ (   var "System.Concurrency.makeChannel"
                       .! ("a", var "Prelude.Types.Nat")
                       .! ("io", var "PrimIO.IO"))
                   .$ (    MkArg
                             MW
                             ExplicitArg
                             (Just "channel_Main_fib_saw_Main_fib_result_0_")
                             implicitFalse
                       .=>    var "Prelude.Interfaces.(>>=)"
                           .$ (   var "System.Concurrency.makeChannel"
                               .! ("a", var "Prelude.Types.Nat")
                               .! ("io", var "PrimIO.IO"))
                           .$ (    MkArg MW ExplicitArg (Just "channel_Main_fib_saw_") implicitFalse
                               .=>    var "Prelude.Interfaces.(>>=)"
                                   .$ (   var "System.Concurrency.makeChannel"
                                       .! ( "a"
                                          ,    var "Builtin.Pair"
                                            .$ var "Prelude.Types.Nat"
                                            .$ (   var "Builtin.Pair"
                                                .$ var "Prelude.Types.Nat"
                                                .$ var "Prelude.Types.Nat")
                                          )
                                       .! ("io", var "PrimIO.IO"))
                                   .$ (    MkArg
                                             MW
                                             ExplicitArg
                                             (Just "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_")
                                             implicitFalse
                                       .=>    var "Prelude.Interfaces.(>>=)"
                                           .$ (   var "Prelude.IO.fork"
                                               .$ (   var "conFib_concurrent_function_1"
                                                   .$ var "channel_Main_fib_Main_fib_result_0_"
                                                   .$ var "channel_Main_fib_saw_Main_fib_result_0_"
                                                   .$ var "channel_Main_fib_saw_"
                                                   .$ var
                                                        "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_"
                                                   .$ var "saw"))
                                           .$ (    MkArg MW ExplicitArg Nothing (var "PrimIO.ThreadID")
                                               .=>    var "Prelude.Interfaces.(>>=)"
                                                   .$ (   var "Prelude.IO.fork"
                                                       .$ (   var "conFib_concurrent_function_2"
                                                           .$ var "channel_Main_fib_Main_fib_result_0_"
                                                           .$ var "channel_Main_fib_saw_Main_fib_result_0_"
                                                           .$ var "channel_Main_fib_saw_"
                                                           .$ var
                                                                "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_"
                                                           .$ var "saw"))
                                                   .$ (    MkArg MW ExplicitArg Nothing (var "PrimIO.ThreadID")
                                                       .=>    var "System.Concurrency.channelGet"
                                                           .! ( "a"
                                                              ,    var "Builtin.Pair"
                                                                .$ var "Prelude.Types.Nat"
                                                                .$ (   var "Builtin.Pair"
                                                                    .$ var "Prelude.Types.Nat"
                                                                    .$ var "Prelude.Types.Nat")
                                                              )
                                                           .! ("io", var "PrimIO.IO")
                                                           .$ var
                                                                "channel__Main_fib_result_0_Main_fib_result_0_Main_fib_result_0_"))))))
      ]
  ]
)






















left: [ 
        Function: "Main.fib" 
            Called with concatanation of: ["saw", "Main_fib_result_0"] 
                concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: ""; 
                    Returns type "Prelude.Types.Nat" Result of type "Prelude.Types.Nat" was saved to: ["Main_fib_result_0"]
                        Dependencies: ["Main_fib_result_0", "Main_fib_result_0", "Main_fib_result_0"]
                            Call sequence number: 0, 
        
        Function: "Main.fib" 
            Called with wraped variable: "Main_fib_result_0"; 
                Returns type "Prelude.Types.Nat" Result of type "Prelude.Types.Nat" was saved to: ["Main_fib_result_0"]
                    Dependencies: ["Main_fib_result_0", "Main_fib_result_0", "Main_fib_result_0"]
                        Call sequence number: 0
      ]
right: [ 
        Function: "" 
            Called with concatanation of: ["Main_fib_result_0", "Main_fib_result_0", "Main_fib_result_0"] 
                concatenated by function: "ConcurrentDsl.Api.Concurrent.concat2" user composition function: ""; 
                    Returns type "Builtin.Pair" .$ var "Prelude.Types.Nat" .$ (   var "Builtin.Pair" .$ var "Prelude.Types.Nat" .$ var "Prelude.Types.Nat") Result of type    var "Builtin.Pair" .$ var "Prelude.Types.Nat" .$ (   var "Builtin.Pair" .$ var "Prelude.Types.Nat" .$ var "Prelude.Types.Nat") was not saved
                        Dependencies: ["Main_fib_result_0", "Main_fib_result_0", "Main_fib_result_0"]
                            Call sequence number: 0, 
         
         Function: "Main.fib" 
            Called with wraped variable: "saw"; 
                Returns type"Prelude.Types.Nat" Result of type "Prelude.Types.Nat" was saved to: ["Main_fib_result_0"]
                    Dependencies: []
                        Call sequence number: 1
       ]





  UID   PID NI PRI  %CPU COMM
    0     1  0  37   0,0 /sbin/launchd
    0    95  0  37   0,0 /usr/libexec/logd
    0    97  0  31   0,0 /usr/libexec/UserEventAgent
    0    99  0  20   0,0 /System/Library/PrivateFrameworks/Uninstall.framework/Resources/uninstalld
    0   100  0  50   0,0 /System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/FSEvents.framework/Versions/A/Support/fseventsd
    0   101  0   4   0,0 /System/Library/PrivateFrameworks/MediaRemote.framework/Support/mediaremoted
    0   102  0   4   0,0 /usr/sbin/systemstats
    0   104  0  37   0,0 /usr/libexec/configd
    0   106  0  37   0,0 /System/Library/CoreServices/powerd.bundle/powerd
    0   111  0  31   0,0 /usr/libexec/remoted
    0   116  0  97   0,0 /usr/libexec/watchdogd
    0   120  0  50   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Support/mds
    0   122  0  31   0,0 /usr/libexec/kernelmanagerd
    0   123  0  37   0,0 /usr/libexec/diskarbitrationd
    0   127  0   4   0,0 /usr/sbin/syslogd
    0   130  0  37   0,0 /usr/libexec/thermalmonitord
    0   131  0  31   0,0 /usr/libexec/opendirectoryd
    0   132  0  31   0,0 /System/Library/PrivateFrameworks/ApplePushService.framework/apsd
    0   133  0   4   0,0 /System/Library/CoreServices/launchservicesd
  266   134  0  31   0,0 /usr/libexec/timed
    0   136  0  31   0,0 /usr/sbin/securityd
    0   141  0  20   0,0 autofsd
    0   142  0   4   0,0 /usr/libexec/dasd
  241   144  0  31   0,0 /usr/sbin/distnoted
    0   146  0   4   0,0 /usr/libexec/PerfPowerServices
    0   148  0  31   0,0 /System/Library/CoreServices/logind
    0   149  0   4   0,0 /System/Library/PrivateFrameworks/GenerationalStorage.framework/Versions/A/Support/revisiond
    0   150  0  37   0,0 /usr/sbin/KernelEventAgent
    0   154  0  31   0,0 /usr/sbin/notifyd
    0   156  0  37   0,0 /usr/libexec/corebrightnessd
    0   157  0  61   0,0 /usr/libexec/AirPlayXPCHelper
    0   159  0   4   0,0 /usr/sbin/cfprefsd
    0   164  0   4   0,0 /System/Library/PrivateFrameworks/CoreDuetContext.framework/Resources/contextstored
    0   165  0  37   0,0 /System/Library/CoreServices/coreservicesd
    0   166  0   4   0,0 /usr/libexec/lsd
    0   169  0   4   0,0 /usr/libexec/runningboardd
  202   201  0  63   0,0 /usr/sbin/coreaudiod
  263   207  0  31   0,0 /System/Library/PrivateFrameworks/CoreAnalytics.framework/Support/analyticsd
    0   210  0  20   0,0 /usr/libexec/apfsd
  242   211  0   4   0,0 /usr/libexec/nsurlsessiond
    0   213  0  31   0,0 /usr/libexec/airportd
    0   225  0  31   0,0 /usr/sbin/distnoted
    0   228  0   4   0,0 /System/Library/Frameworks/Security.framework/Versions/A/XPCServices/authd.xpc/Contents/MacOS/authd
    0   230  0  31   0,0 /usr/libexec/syspolicyd
   88   236  0  31   0,0 /usr/sbin/distnoted
  202   237  0  31   0,0 /usr/sbin/distnoted
   65   242  0  31   0,0 /usr/sbin/mDNSResponder
   24   246  0   4   0,0 /usr/libexec/symptomsd
    0   247  0   4   0,0 /System/Library/PrivateFrameworks/WirelessDiagnostics.framework/Support/awdd
    0   251  0  31   0,0 /usr/sbin/mDNSResponderHelper
    0   252  0  55   0,3 /usr/libexec/TouchBarServer
    0   257  0   4   0,0 /usr/libexec/nesessionmanager
  262   312  0  31   0,0 /usr/sbin/distnoted
  278   314  0  31   0,0 /usr/sbin/distnoted
  200   320  0  31   0,0 /usr/sbin/distnoted
  262   341  0  37   0,0 /System/Library/Frameworks/CoreMediaIO.framework/Versions/A/Resources/UVCAssistant.systemextension/Contents/MacOS/UVCAssistant
    0   345  0   4   0,0 /usr/libexec/searchpartyd
  270   355  0  63   0,0 /System/Library/DriverExtensions/com.apple.DriverKit-IOUserDockChannelSerial.dext/com.apple.DriverKit-IOUserDockChannelSerial
  270   356  0  63   0,0 /System/Library/DriverExtensions/com.apple.AppleUserHIDDrivers.dext/com.apple.AppleUserHIDDrivers
    0   368  0  31   0,0 /usr/libexec/diskmanagementd
    0   463  0  61   0,0 /usr/sbin/systemsoundserverd
    0   464  0   4   0,0 /System/Library/Frameworks/AudioToolbox.framework/AudioComponentRegistrar
    0   465  0   4   0,0 /System/Library/Frameworks/AudioToolbox.framework/XPCServices/com.apple.audio.SandboxHelper.xpc/Contents/MacOS/com.apple.audio.SandboxHelper
   55   469  0  31   0,0 /System/Library/CoreServices/appleeventsd
  205   473  0  31   0,0 /usr/sbin/distnoted
  205   482  0   4   0,0 /usr/libexec/locationd
    0   488  0  20   0,0 /Library/Application Support/LogiFacecam.bundle/Contents/MacOS/LogiFacecamService
    0   496  0  20   0,0 /Library/PrivilegedHelperTools/com.docker.vmnetd
    0   498  0   4   0,0 /usr/libexec/wifianalyticsd
    0   504  0  31   0,0 /usr/sbin/audioclocksyncd
  242   520  0  31   0,0 /usr/sbin/distnoted
    0   664  0  37   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Versions/A/Support/mds_stores
   89   674  0  31   0,0 /usr/sbin/distnoted
  441   678  0  31   0,0 /usr/libexec/rosetta/oahd
  501   682  0  31   0,0 /usr/sbin/distnoted
    0   688  0  31   0,0 /Library/SystemExtensions/713B01DE-DDC6-4B2B-9214-3672E97F9356/com.checkpoint.fw.filter.systemextension/Contents/MacOS/com.checkpoint.fw.filter
  273   691  0  31   0,0 /usr/sbin/distnoted
  501   693  0  46   0,0 /System/Library/PrivateFrameworks/SkyLight.framework//Versions/A/Resources/AquaAppearanceHelper.app/Contents/MacOS/AquaAppearanceHelper
  235   694  0  31   0,0 /usr/sbin/distnoted
    0   699  0   4   0,0 /System/Library/Frameworks/LocalAuthentication.framework/Support/coreauthd
    0   700  0  20   0,0 /usr/libexec/securityd_service
    0   701  0  31   0,0 /Library/SystemExtensions/7AA986B5-0664-4254-92A3-B7733C6633B7/com.trendmicro.icore.es.systemextension/Contents/MacOS/com.trendmicro.icore.es
    0   710  0   4   0,0 /usr/sbin/systemstats
  262   711  0  48   0,0 /usr/sbin/appleh13camerad
   55   734  0  31   0,0 /usr/sbin/distnoted
    0   765  0   4   0,0 /System/Library/CoreServices/sharedfilelistd
  501   784  0   4   0,0 /usr/libexec/lsd
  247   856  0  31   0,0 /usr/sbin/distnoted
    0   958  0   4   0,0 /usr/libexec/aned
    0   959  0   4   0,0 /usr/libexec/symptomsd-diag
  501   960  0   4   0,0 /System/Library/PrivateFrameworks/IMDPersistence.framework/XPCServices/IMDPersistenceAgent.xpc/Contents/MacOS/IMDPersistenceAgent
    0   961  0   4   0,0 /System/Library/PrivateFrameworks/XprotectFramework.framework/Versions/A/XPCServices/XProtectBehaviorService.xpc/Contents/MacOS/XProtectBehaviorService
  501   962  0   4   0,0 /System/Library/PrivateFrameworks/CoreSuggestions.framework/Versions/A/Support/suggestd
  501   963  0   4   0,0 /System/Library/CoreServices/ScopedBookmarkAgent
  501   964  0   4   0,0 /System/Library/PrivateFrameworks/SocialLayer.framework/sociallayerd.app/Contents/MacOS/sociallayerd
  501   966  0   4   0,0 /System/Library/PrivateFrameworks/ContextKit.framework/Versions/A/XPCServices/ContextService.xpc/Contents/MacOS/ContextService
  501   968  0   4   0,0 /System/Library/PrivateFrameworks/PhotoAnalysis.framework/Versions/A/Support/photoanalysisd
    0   969  0   4   0,0 /usr/libexec/dprivacyd
    0   970  0   4   0,0 /System/Library/PrivateFrameworks/AppleNeuralEngine.framework/XPCServices/ANEStorageMaintainer.xpc/Contents/MacOS/ANEStorageMaintainer
  501   972  0  46   0,0 /System/Library/CoreServices/CoreServicesUIAgent.app/Contents/MacOS/CoreServicesUIAgent
  501   976  0   4   0,0 /System/Library/PrivateFrameworks/FileProvider.framework/Support/fileproviderd
  501   977  0   4   0,0 /usr/libexec/promotedcontentd
    0   978  0   4   0,0 /System/Library/CoreServices/SubmitDiagInfo
  260   979  0  50   0,0 /usr/libexec/nfcd
  501   981  0   4   0,0 cloudphotod
  501   995 17   4   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Versions/A/Support/mdbulkimport
  260   998  0  31   0,0 /usr/sbin/distnoted
  501  1005  0   4   0,0 /System/Library/PrivateFrameworks/ContactsDonation.framework/Versions/A/Support/contactsdonationagent
  501  1016  0   4   0,0 /Library/Apple/System/Library/CoreServices/XProtect.app/Contents/MacOS/XProtect
  501  1017  0   4   0,0 /System/Library/PrivateFrameworks/AvatarPersistence.framework/Support/avatarsd
  501  1018  0   4   0,0 /Library/Apple/System/Library/CoreServices/XProtect.app/Contents/XPCServices/XProtectPluginService.xpc/Contents/MacOS/XProtectPluginService
    0  1142  0   4   0,0 /System/Library/CoreServices/CrashReporterSupportHelper
  265  1194  0  37   0,0 /System/Library/PrivateFrameworks/CoreFP.framework/Versions/A/fairplayd
   89  1279 17   4   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Versions/A/Support/mdbulkimport
  277  1432  0  31   0,0 /usr/sbin/distnoted
  501  1629  0  60   0,0 /System/Library/CoreServices/PowerChime.app/Contents/MacOS/PowerChime
  501  1717  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501  2074  0  31   0,0 /System/Library/Services/AppleSpell.service/Contents/MacOS/AppleSpell
  501  2075  0   4   0,0 /usr/libexec/keyboardservicesd
    0  2108  0  31   0,0 automountd
    0  2138  0  31   0,0 /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/CVMServer
  501  2358  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
    0  2598  0   4   0,0 /usr/libexec/misagent
  501  5695  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501  8704  0   4   0,0 /System/Library/PrivateFrameworks/IDSBlastDoorSupport.framework/Versions/A/XPCServices/IDSBlastDoorService.xpc/Contents/MacOS/IDSBlastDoorService
  282  9184  0   4   0,0 /usr/libexec/trustd
  270 11274  0  63   0,0 /System/Library/DriverExtensions/IOUserBluetoothSerialDriver.dext/IOUserBluetoothSerialDriver
  270 11285  0  63   0,0 /System/Library/DriverExtensions/IOUserBluetoothSerialDriver.dext/IOUserBluetoothSerialDriver
  501 11987  0  20   0,0 /System/Cryptexes/App/System/Library/CoreServices/SafariSupport.bundle/Contents/MacOS/SafariBookmarksSyncAgent
  501 11992  0  20   0,0 /System/Cryptexes/App/usr/libexec/SafariLaunchAgent
  501 12031  0   4   0,0 /System/Library/Frameworks/AudioToolbox.framework/AudioComponentRegistrar
  501 12032  0   4   0,0 /System/Library/Frameworks/AudioToolbox.framework/XPCServices/CarbonComponentScannerXPC.xpc/Contents/MacOS/CarbonComponentScannerXPC
    0 12035  0   4   0,0 /System/Library/PrivateFrameworks/AppleNeuralEngine.framework/XPCServices/ANECompilerService.xpc/Contents/MacOS/ANECompilerService
  501 13241  0   4   0,0 /usr/libexec/USBAgent
  501 13519  0   4   0,0 /System/Library/PrivateFrameworks/IMDPersistence.framework/IMAutomaticHistoryDeletionAgent.app/Contents/MacOS/IMAutomaticHistoryDeletionAgent
  501 14685  0   4   0,0 /System/Library/PrivateFrameworks/AppStoreDaemon.framework/Support/appstoreagent
  274 14686  0   4   0,0 /System/Library/PrivateFrameworks/InstallCoordination.framework/Support/installcoordinationd
    0 14688  0   4   0,0 /System/Library/PrivateFrameworks/AppStoreDaemon.framework/Versions/A/XPCServices/com.apple.AppStoreDaemon.StorePrivilegedTaskService.xpc/Contents/MacOS/com.apple.AppStoreDaemon.StorePrivilegedTaskService
  501 14775  0   4   0,0 /System/Library/PrivateFrameworks/CacheDelete.framework/deleted
  501 16392  0  31   0,0 /Applications/Telegram.app/Contents/Helpers/crashpad_handler
    0 16945  0   4   0,0 /System/Library/CoreServices/osanalyticshelper
  501 19076  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 19111  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 19173  0  31   0,1 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 19218  0   4   0,0 /System/Library/PrivateFrameworks/ProtectedCloudStorage.framework/Helpers/ProtectedCloudKeySyncing
  501 19219  0   4   0,0 /System/Library/PrivateFrameworks/CloudServices.framework/Helpers/com.apple.sbd
  501 19248  0  31   0,1 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 19252  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
    0 19282  0   4   0,0 /System/Library/PrivateFrameworks/SystemStatusServer.framework/Support/systemstatusd
  501 19330  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 19368  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 19398  0  37   0,0 /System/Library/Frameworks/QuickLookThumbnailing.framework/Support/com.apple.quicklook.ThumbnailsAgent
  501 19401  0   4   0,0 /System/Library/Frameworks/NetFS.framework/Versions/A/XPCServices/PlugInLibraryService.xpc/Contents/MacOS/PlugInLibraryService
  501 19404  0   4   0,0 /System/Library/PrivateFrameworks/XprotectFramework.framework/Versions/A/XPCServices/XprotectService.xpc/Contents/MacOS/XprotectService
    0 19406  0   4   0,0 /System/Library/PrivateFrameworks/CacheDelete.framework/deleted_helper
  501 19407  0   4   0,0 /usr/libexec/replayd
    0 19409  0   4   0,0 /System/Library/PrivateFrameworks/PackageKit.framework/Resources/installd
    0 19410  0   4   0,0 /System/Library/PrivateFrameworks/PackageKit.framework/Resources/system_installd
  501 19411  0   4   0,0 /System/Library/PrivateFrameworks/GeoServices.framework/geodMachServiceBridge
    0 19412  0   4   0,0 /System/Library/PrivateFrameworks/CoreSymbolication.framework/coresymbolicationd
  235 19413  0   4   0,0 /usr/libexec/AssetCache/AssetCache
    0 19415  0   4   0,0 /System/Library/PrivateFrameworks/AssetCacheServicesExtensions.framework/XPCServices/AssetCacheTetheratorService.xpc/Contents/MacOS/AssetCacheTetheratorService
    0 19434  0  37   0,0 /usr/libexec/biometrickitd
  501 19435  0  46   0,0 /System/Library/Frameworks/LocalAuthentication.framework/Support/coreautha.bundle/Contents/MacOS/coreautha
    0 19439  0   4   0,0 /System/Library/Frameworks/Metal.framework/Versions/A/XPCServices/MTLCompilerService.xpc/Contents/MacOS/MTLCompilerService
    0 19441  0   4   0,0 /System/Library/PrivateFrameworks/AccountPolicy.framework/XPCServices/com.apple.AccountPolicyHelper.xpc/Contents/MacOS/com.apple.AccountPolicyHelper
    0 19444  0   4   0,0 /System/Library/PrivateFrameworks/PackageKit.framework/Versions/A/XPCServices/package_script_service.xpc/Contents/MacOS/package_script_service
  501 19529  0   4   0,0 /usr/libexec/naturallanguaged
  501 19856  0   4   0,0 /usr/libexec/fmfd
  501 19858  0   4   0,0 /System/Library/PrivateFrameworks/DoNotDisturbServer.framework/Support/donotdisturbd
  501 19859  0   4   0,0 /System/Library/PrivateFrameworks/CommunicationsFilter.framework/CMFSyncAgent
  501 19944  0  31   0,4 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 20028  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 20427  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 20798  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper.app/Contents/MacOS/Google Chrome Helper
  501 20805  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
    0 20999  0   4   0,0 /System/Library/PrivateFrameworks/BackgroundTaskManagement.framework/Resources/backgroundtaskmanagementd
  222 21217 20   4   0,0 /usr/sbin/netbiosd
  501 21382  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 21746  0  20   0,0 /usr/bin/ssh-agent
  501 22652  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 26839  0  31   0,0 /Applications/Telegram.app/Contents/Helpers/crashpad_handler
  274 29405  0  31   0,0 /usr/sbin/distnoted
  501 34734  0   4   0,0 /System/Library/PrivateFrameworks/DistributedEvaluation.framework/Versions/A/XPCServices/com.apple.siri-distributed-evaluation.xpc/Contents/MacOS/com.apple.siri-distributed-evaluation
  501 34735  0   4   0,0 /usr/libexec/mlruntimed
    0 39182  0   4   0,0 /Library/Apple/System/Library/CoreServices/XProtect.app/Contents/MacOS/XProtect
    0 39183  0   4   0,0 /Library/Apple/System/Library/CoreServices/XProtect.app/Contents/XPCServices/XProtectPluginService.xpc/Contents/MacOS/XProtectPluginService
  501 39460  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 39589  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 39602  0   4   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Versions/A/Support/mdworker_shared
    0 43973  0  20   0,0 /Library/PrivilegedHelperTools/com.xk72.charles.ProxyHelper
  213 48930  0  31   0,0 /System/Library/PrivateFrameworks/MobileDevice.framework/Versions/A/Resources/usbmuxd
  501 52649  0   4   0,0 /usr/libexec/nsurlsessiond
    0 61252  0   4   0,0 /System/Library/Frameworks/Security.framework/Versions/A/XPCServices/com.apple.CodeSigningHelper.xpc/Contents/MacOS/com.apple.CodeSigningHelper
    0 64332  0   4   0,0 /System/Library/Frameworks/AudioToolbox.framework/XPCServices/CAReportingService.xpc/Contents/MacOS/CAReportingService
    0 68208  0  37   0,0 /usr/sbin/bluetoothd
  501 71421  0  31   0,0 /Applications/Telegram.app/Contents/Helpers/crashpad_handler
  501 72096  0   4   0,0 /usr/sbin/cfprefsd
   92 72209  0  31   0,0 /usr/sbin/distnoted
    0 72283  0  31   0,0 /usr/sbin/filecoordinationd
    0 72478  0   4   0,1 /usr/libexec/sysmond
    0 72860  0  20   0,0 /Library/Application Support/Citrix Receiver/ctxworkspaceupdater
  501 76494  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 76787  0  31   0,0 /Applications/Telegram.app/Contents/Helpers/crashpad_handler
   88 80264  0  79   2,0 /System/Library/PrivateFrameworks/SkyLight.framework/Resources/WindowServer
  501 80278  0  47   0,0 /System/Library/CoreServices/loginwindow.app/Contents/MacOS/loginwindow
  501 80350  0  31   0,0 /usr/libexec/UserEventAgent
  501 80352  0  46   0,0 /System/Library/CoreServices/ControlCenter.app/Contents/MacOS/ControlCenter
  501 80358  0  47   0,0 /System/Library/CoreServices/WindowManager.app/Contents/MacOS/WindowManager
  501 80367  0  37   0,0 /System/Library/CoreServices/lockoutagent
  501 80369  0  31   0,0 /System/Library/CoreServices/ControlStrip.app/Contents/MacOS/ControlStrip
  501 80370  0  31   0,0 /usr/sbin/usernoted
  501 80372  0  31   0,0 /usr/libexec/rapportd
  501 80376  0  31   0,0 /usr/libexec/pboard
  501 80379  0  31   0,0 /System/Library/PrivateFrameworks/VoiceShortcuts.framework/Versions/A/Support/siriactionsd
  501 80383  0  31   0,0 /System/Library/PrivateFrameworks/IDS.framework/identityservicesd.app/Contents/MacOS/identityservicesd
  501 80385  0  46   0,0 /Applications/Telegram.app/Contents/MacOS/Telegram
  501 80386  0  46   0,0 /Applications/Google Chrome.app/Contents/MacOS/Google Chrome
  501 80391  0  47   0,3 /System/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal
  501 80394  0   4   0,0 /Applications/Keynote.app/Contents/MacOS/Keynote
  501 80395  0  46   0,0 /Applications/Visual Studio Code.app/Contents/MacOS/Electron
  501 80396  0   4   0,0 /System/Applications/Preview.app/Contents/MacOS/Preview
  501 80397  0  46   0,9 /System/Applications/Utilities/Activity Monitor.app/Contents/MacOS/Activity Monitor
  501 80398  0  47   0,0 /System/Library/CoreServices/Dock.app/Contents/MacOS/Dock
  501 80399  0  47   0,0 /System/Library/CoreServices/SystemUIServer.app/Contents/MacOS/SystemUIServer
  501 80400  0  46   0,0 /System/Library/CoreServices/Finder.app/Contents/MacOS/Finder
  501 80406  0   4   0,0 /System/Library/Frameworks/ApplicationServices.framework/Frameworks/ATS.framework/Support/fontd
  501 80413  0  31   0,0 /Applications/Telegram.app/Contents/Helpers/crashpad_handler
  501 80415  0  31   0,0 /System/Library/PrivateFrameworks/CloudDocsDaemon.framework/Versions/A/Support/bird
    0 80416  0  31   0,0 /System/Library/PrivateFrameworks/AmbientDisplay.framework/Versions/A/XPCServices/com.apple.AmbientDisplayAgent.xpc/Contents/MacOS/com.apple.AmbientDisplayAgent
  501 80417  0  31   0,0 /System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ATS.framework/Versions/A/Support/fontworker
  501 80425  0  46   0,0 /System/Library/CoreServices/Spotlight.app/Contents/MacOS/Spotlight
  501 80427  0   4   0,0 /System/Library/CoreServices/sharedfilelistd
  501 80430  0   4   0,0 /System/Library/PrivateFrameworks/AMPDevices.framework/Versions/A/Support/AMPDeviceDiscoveryAgent
  501 80431  0   4   0,0 /System/Library/PrivateFrameworks/UserActivity.framework/Agents/useractivityd
  501 80432  0  46   0,0 /System/Library/CoreServices/Dock.app/Contents/XPCServices/com.apple.dock.extra.xpc/Contents/MacOS/com.apple.dock.extra
  501 80439  0  31   0,0 /System/Library/Frameworks/InputMethodKit.framework/Resources/imklaunchagent
  501 80443  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/chrome_crashpad_handler
  501 80450  0  31   0,0 /usr/libexec/sharingd
  501 80455  0  54   0,5 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (GPU).app/Contents/MacOS/Google Chrome Helper (GPU)
  501 80456  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper.app/Contents/MacOS/Google Chrome Helper
  501 80461  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper.app/Contents/MacOS/Google Chrome Helper
  501 80467  0   4   0,0 /System/Library/PrivateFrameworks/FamilyCircle.framework/Versions/A/Resources/familycircled
  501 80470  0  46   0,0 /System/Library/CoreServices/NowPlayingTouchUI.app/Contents/MacOS/NowPlayingTouchUI
  501 80474  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80475  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80476  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80479  0  37   0,0 /System/Library/CoreServices/WiFiAgent.app/Contents/MacOS/WiFiAgent
  501 80480  0   4   0,0 /System/Library/Input Methods/PressAndHold.app/Contents/PlugIns/PAH_Extension.appex/Contents/MacOS/PAH_Extension
  501 80484  0   4   0,0 /System/Library/PrivateFrameworks/CommerceKit.framework/Versions/A/Resources/commerce
  501 80485  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80489  0   4   0,0 /System/Library/Input Methods/EmojiFunctionRowIM.app/Contents/PlugIns/EmojiFunctionRowIM_Extension.appex/Contents/MacOS/EmojiFunctionRowIM_Extension
  501 80491  0  46   0,0 /System/Library/CoreServices/NotificationCenter.app/Contents/MacOS/NotificationCenter
  501 80495  0  31   0,0 /System/Library/PrivateFrameworks/IMCore.framework/imagent.app/Contents/MacOS/imagent
    0 80496  0  20   0,0 /Library/Application Support/Checkpoint/Endpoint Connect/TracSrvWrapper
  501 80504  0  31   0,0 /usr/libexec/avconferenced
  501 80505  0   4   0,0 /System/Library/PrivateFrameworks/MediaRemote.framework/Support/mediaremoteagent
  501 80539  0   4   0,0 /System/Library/PrivateFrameworks/AssistantServices.framework/Versions/A/Support/assistantd
  501 80555  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80557  0  31   0,1 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80559  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80561  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80563  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80566  0   4   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Versions/A/Support/corespotlightd
  501 80573  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80574  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80575  0  20   0,0 /System/Library/Frameworks/CoreServices.framework/Frameworks/Metadata.framework/Versions/A/Support/mdwrite
  501 80576  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80577  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 80588  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Electron Framework.framework/Helpers/chrome_crashpad_handler
  501 80589  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper (GPU).app/Contents/MacOS/Code Helper (GPU)
  501 80591  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper.app/Contents/MacOS/Code Helper
  501 80592  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper (Renderer).app/Contents/MacOS/Code Helper (Renderer)
  501 80613  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper.app/Contents/MacOS/Code Helper
  501 80614  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper.app/Contents/MacOS/Code Helper
  501 80615  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper (Plugin).app/Contents/MacOS/Code Helper (Plugin)
  501 80617  0  31   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Code Helper.app/Contents/MacOS/Code Helper
  501 80619  0  31   0,0 /System/Library/Image Capture/Support/icdd
  501 80654  0  31   0,0 /bin/sh
  501 80715  0  31   0,0 /bin/sh
  501 80718  0  31   0,0 /usr/local/bin/chez
  501 80759  0   4   0,0 /System/Library/PrivateFrameworks/PassKitCore.framework/passd
  501 80766  0   4   0,0 /System/Library/PrivateFrameworks/CommerceKit.framework/Versions/A/Resources/storedownloadd
  501 80769  0   4   0,0 /System/Library/CoreServices/diagnostics_agent
  501 80830  0  31   0,0 /System/Library/PrivateFrameworks/CoreSpeech.framework/corespeechd
  501 80845  0  46   0,0 /System/Library/CoreServices/AirPlayUIAgent.app/Contents/MacOS/AirPlayUIAgent
  501 80849  0  46   0,0 /System/Library/CoreServices/TextInputMenuAgent.app/Contents/MacOS/TextInputMenuAgent
  501 80852  0  20   0,0 /usr/local/libexec/ServiceRecords.app/Contents/MacOS/ServiceRecords
    0 80854  0   4   0,0 /System/Library/Frameworks/CryptoTokenKit.framework/ctkahp.bundle/Contents/MacOS/ctkahp
  501 80860  0   4   0,0 /System/Library/Frameworks/CryptoTokenKit.framework/ctkahp.bundle/Contents/MacOS/ctkahp
    0 80875  0  31   0,0 /usr/sbin/WirelessRadioManagerd
  501 80901  0  61   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper.app/Contents/MacOS/Google Chrome Helper
  501 80984  0   4   0,0 /usr/libexec/ContinuityCaptureAgent
  501 81084  0  20   0,0 /usr/local/libexec/AuthManager_Mac.app/Contents/MacOS/AuthManager_Mac
  501 81197 -1  20   0,0 /Applications/Visual Studio Code.app/Contents/Frameworks/Squirrel.framework/Resources/ShipIt
  501 81232  0   4   0,0 /usr/libexec/ptpcamerad
  501 81233  0   4   0,0 /System/Library/Frameworks/ImageCaptureCore.framework/Versions/A/XPCServices/mscamerad-xpc.xpc/Contents/MacOS/mscamerad-xpc
    0 85461  0  20   0,0 /Library/PrivilegedHelperTools/com.trendmicro.AFM.HelperTool
    0 89766  0  31   0,0 /Library/Developer/PrivateFrameworks/CoreSimulator.framework/Resources/bin/simdiskimaged
  501 90348  0  31  24,9 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 90352  0   4   0,0 /System/Library/Frameworks/VideoToolbox.framework/Versions/A/XPCServices/VTDecoderXPCService.xpc/Contents/MacOS/VTDecoderXPCService
  200 90946  0   4   0,0 /System/Library/CoreServices/Software Update.app/Contents/Resources/softwareupdated
    0 90950  0   4   0,0 /System/Library/CoreServices/Software Update.app/Contents/Resources/suhelperd
  501 93606  0   4   0,0 /System/Library/PrivateFrameworks/PhotoLibraryServices.framework/Versions/A/Support/photolibraryd
    0 94181  0   4   0,0 /usr/libexec/nehelper
  501 94188  0   4   0,0 /System/Library/PrivateFrameworks/CoreDuetContext.framework/Resources/ContextStoreAgent
  501 98157  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 98309  0   4   0,0 /System/Library/PrivateFrameworks/BiomeStreams.framework/Support/BiomeAgent
    0 98322  0  20   0,0 /usr/libexec/rtcreportingd
  289 98324  0   4   0,0 /System/Library/PrivateFrameworks/BiomeStreams.framework/Support/biomed
    0 98325  0  31   0,0 /usr/libexec/online-authd
    0 98328  0   4   0,0 /System/Library/Frameworks/CoreMediaIO.framework/Versions/A/Resources/com.apple.cmio.registerassistantservice
    0 98329  0   4   0,0 /System/Library/Frameworks/SystemExtensions.framework/Versions/A/Helpers/sysextd
  501 98330  0   4   0,0 /usr/libexec/trustd
    0 98331  0   4   0,0 /usr/libexec/trustdFileHelper
    0 98334  0   4   0,0 /System/Library/CoreServices/backupd.bundle/Contents/Resources/backupd-helper
    0 98335  0   4   0,0 /System/Library/CoreServices/backupd.bundle/Contents/Resources/backupd
  247 98337  0   4   0,0 /usr/libexec/gamecontrollerd
  501 98338  0  20   0,0 /usr/libexec/gamecontrolleragentd
  501 98340  0   4   0,0 /System/Library/Frameworks/CoreSpotlight.framework/CoreSpotlightService
    0 98342  0   4   0,0 /usr/libexec/mmaintenanced
  501 98343  0   4   0,0 /System/Library/PrivateFrameworks/ScreenTimeCore.framework/Versions/A/ScreenTimeAgent
  501 98344  0   4   0,0 /usr/libexec/secinitd
  501 98345  0   4   0,0 /usr/libexec/containermanagerd
    0 98346  0  31   0,0 /usr/libexec/usermanagerd
  501 98349  0   4   0,0 /System/Library/Frameworks/Accounts.framework/Versions/A/Support/accountsd
  501 98350  0   4   0,0 /usr/libexec/dmd
  501 98351  0  37   0,0 /System/Library/PrivateFrameworks/ViewBridge.framework/Versions/A/XPCServices/ViewBridgeAuxiliary.xpc/Contents/MacOS/ViewBridgeAuxiliary
  501 98352  0   4   0,0 /System/Library/PrivateFrameworks/CloudKitDaemon.framework/Support/cloudd
  501 98353  0   4   0,0 /System/Library/PrivateFrameworks/TCC.framework/Support/tccd
    0 98354  0  31   0,0 /usr/libexec/sandboxd
  501 98355  0  31   0,0 /usr/libexec/secd
  501 98356  0   4   0,0 /System/Library/Frameworks/Security.framework/Versions/A/XPCServices/TrustedPeersHelper.xpc/Contents/MacOS/TrustedPeersHelper
  501 98357  0   4   0,0 /System/Library/PrivateFrameworks/CoreCDP.framework/Versions/A/Resources/cdpd
  501 98358  0   4   0,0 /System/Library/Frameworks/Security.framework/Versions/A/Resources/CloudKeychainProxy.bundle/Contents/MacOS/CloudKeychainProxy
  501 98359  0   4   0,0 /System/Library/PrivateFrameworks/SyncedDefaults.framework/Support/syncdefaultsd
    0 98361  0   4   0,0 /usr/libexec/coreduetd
  501 98363  0   4   0,0 /usr/libexec/knowledge-agent
  501 98364  0   4   0,0 /System/Library/Frameworks/Contacts.framework/Support/contactsd
  501 98366  0   4   0,0 /System/Library/PrivateFrameworks/CalendarDaemon.framework/Support/calaccessd
    0 98367  0   4   0,0 /usr/libexec/mobileassetd
  501 98368  0  37   0,0 /System/Library/CoreServices/CoreLocationAgent.app/Contents/MacOS/CoreLocationAgent
  501 98369  0   4   0,0 /usr/libexec/remindd
  501 98375  0  37   0,0 /System/Library/Frameworks/CoreTelephony.framework/Support/CommCenter
  501 98376  0   4   0,0 /System/Library/PrivateFrameworks/TelephonyUtilities.framework/callservicesd
  501 98377  0   4   0,0 /System/Library/PrivateFrameworks/AuthKit.framework/Versions/A/Support/akd
  265 98378  0   4   0,0 /System/Library/PrivateFrameworks/CoreADI.framework/adid
  268 98379  0   4   0,0 /usr/libexec/nearbyd
  501 98380  0   4   0,0 /System/Library/PrivateFrameworks/TelephonyUtilities.framework/XPCServices/com.apple.FaceTime.FTConversationService.xpc/Contents/MacOS/com.apple.FaceTime.FTConversationService
    0 98384  0  37   0,0 /usr/libexec/wifip2pd
  501 98402  0   4   0,0 /System/Library/CoreServices/pbs
  501 98434  0  31   0,0 /Applications/Google Chrome.app/Contents/Frameworks/Google Chrome Framework.framework/Versions/112.0.5615.137/Helpers/Google Chrome Helper (Renderer).app/Contents/MacOS/Google Chrome Helper (Renderer)
  501 98476  0   4   0,0 /System/Library/PrivateFrameworks/Synapse.framework/Support/contentlinkingd
    0 98535  0  37   0,0 /usr/libexec/PowerUIAgent
  501 98536  0   4   0,0 /usr/libexec/triald
  501 98567  0  37   0,0 /System/Library/PrivateFrameworks/MobileTimer.framework/Executables/mobiletimerd
  501 98572  0   4   0,0 /System/Library/PrivateFrameworks/SafariSafeBrowsing.framework/com.apple.Safari.SafeBrowsing.Service
  501 98573  0   4   0,0 /System/Library/PrivateFrameworks/GeoServices.framework/Versions/A/XPCServices/com.apple.geod.xpc/Contents/MacOS/com.apple.geod
    0 98574  0   4   0,0 /System/Library/PrivateFrameworks/GeoServices.framework/Versions/A/XPCServices/com.apple.geod.xpc/Contents/MacOS/com.apple.geod
  262 98575  0   4   0,0 /System/Library/PrivateFrameworks/GeoServices.framework/Versions/A/XPCServices/com.apple.geod.xpc/Contents/MacOS/com.apple.geod
  205 98576  0   4   0,0 /System/Library/PrivateFrameworks/GeoServices.framework/Versions/A/XPCServices/com.apple.geod.xpc/Contents/MacOS/com.apple.geod
    0 98577  0   4   0,0 /usr/libexec/secinitd
  262 98578  0   4   0,0 /usr/libexec/secinitd
  205 98579  0   4   0,0 /usr/libexec/secinitd
  262 98580  0   4   0,0 /usr/sbin/cfprefsd
    0 98581  0   4   0,0 /usr/libexec/containermanagerd
  262 98582  0   4   0,0 /usr/libexec/trustd
  501 98583  0   4   0,0 /usr/libexec/networkserviceproxy
    0 98584  0   4   0,0 /usr/libexec/containermanagerd
  262 98585  0   4   0,0 /usr/libexec/containermanagerd
  501 98587  0  31   0,0 /usr/libexec/neagent
  205 98588  0   4   0,0 /usr/sbin/cfprefsd
  205 98589  0   4   0,0 /usr/libexec/trustd
  205 98590  0   4   0,0 /usr/libexec/containermanagerd
  501 98758  0  46   0,0 /System/Library/CoreServices/OSDUIHelper.app/Contents/MacOS/OSDUIHelper
  501 98891  0   4   0,0 /usr/libexec/routined
    0 99048  0   4   0,0 /System/Library/PrivateFrameworks/CloudKitDaemon.framework/Support/cloudd
  501 99049  0   4   0,0 /System/Library/PrivateFrameworks/iTunesCloud.framework/Support/itunescloudd
  501 99050  0   4   0,0 /System/Library/CoreServices/mapspushd
  501 99051  0   4   0,0 /System/Library/PrivateFrameworks/AppleMediaServices.framework/Resources/amsaccountsd
  501 99052  0   4   0,0 /System/Library/PrivateFrameworks/AppleMediaServicesUI.framework/amsengagementd
  501 99053  0   4   0,0 /System/Library/PrivateFrameworks/HomeKitDaemon.framework/Support/homed
  501 99055  0   4   0,0 /usr/libexec/swcd
  501 99056  0   4   0,0 /System/Library/PrivateFrameworks/AssetCacheServices.framework/Versions/A/XPCServices/AssetCacheLocatorService.xpc/Contents/MacOS/AssetCacheLocatorService
  501 99059  0   4   0,0 /usr/libexec/siriknowledged
    0 99075  0   4   0,0 /usr/libexec/powerdatad
  501 99076  0   4   0,0 /usr/libexec/tipsd
  501 99077  0   4   0,0 /usr/libexec/proactiveeventtrackerd
  200 99080  0   4   0,0 /System/Library/PrivateFrameworks/MobileSoftwareUpdate.framework/Support/softwareupdated
    0 99081  0   4   0,0 /System/Library/PrivateFrameworks/MobileSoftwareUpdate.framework/Versions/A/XPCServices/com.apple.MobileSoftwareUpdate.CleanupPreparePathService.xpc/Contents/MacOS/com.apple.MobileSoftwareUpdate.CleanupPreparePathService
  501 99083  0   4   0,0 /System/Library/PrivateFrameworks/IASUtilities.framework/Versions/A/Resources/installerauthagent
  200 99084  0   4   0,0 /usr/libexec/NRDUpdated
    0 99085  0   4   0,0 /private/var/db/com.apple.xpc.roleaccountd.staging/com.apple.NRD.UpdateBrainService.16777234.154946739.xpc/Contents/MacOS/com.apple.NRD.UpdateBrainService
    0 99086  0   4   0,0 /usr/libexec/xpcroleaccountd
  200 99088  0  55   0,0 /System/Library/PrivateFrameworks/UIFoundation.framework/Versions/A/XPCServices/nsattributedstringagent.xpc/Contents/MacOS/nsattributedstringagent
  200 99090  0   4   0,0 /System/Library/Frameworks/MediaAccessibility.framework/Versions/A/XPCServices/com.apple.accessibility.mediaaccessibilityd.xpc/Contents/MacOS/com.apple.accessibility.mediaaccessibilityd
  200 99091  0   4   0,0 /usr/sbin/cfprefsd
  501 99095  0  37   0,0 /System/Library/PrivateFrameworks/SoftwareUpdate.framework/Resources/SoftwareUpdateNotificationManager.app/Contents/MacOS/SoftwareUpdateNotificationManager
  501 99098  0   4   0,0 /System/Library/PrivateFrameworks/CoreFollowUp.framework/Versions/A/Support/followupd
    0 99100  0   4   0,0 /usr/libexec/smd
  501 99103  0  55   0,0 /System/Library/PrivateFrameworks/UIFoundation.framework/Versions/A/XPCServices/nsattributedstringagent.xpc/Contents/MacOS/nsattributedstringagent
  501 99105  0   4   0,0 /usr/libexec/pkd
  501 99107  0   4   0,0 /System/Library/Frameworks/MediaAccessibility.framework/Versions/A/XPCServices/com.apple.accessibility.mediaaccessibilityd.xpc/Contents/MacOS/com.apple.accessibility.mediaaccessibilityd
  501 99379  0   4   0,0 /usr/libexec/biomesyncd
  501 99505  0   4   0,0 /System/Library/Frameworks/ClassKit.framework/Versions/A/progressd
  501 99621  0   4   0,0 /System/Library/Frameworks/Metal.framework/Versions/A/XPCServices/MTLCompilerService.xpc/Contents/MacOS/MTLCompilerService
  501 99783  0   4   0,0 /System/Library/Frameworks/VideoToolbox.framework/Versions/A/XPCServices/VTDecoderXPCService.xpc/Contents/MacOS/VTDecoderXPCService
  501 99839  0   4   0,0 /System/Library/Frameworks/NetFS.framework/Versions/A/XPCServices/PlugInLibraryService.xpc/Contents/MacOS/PlugInLibraryService
  501 99840  0  46   0,2 /Applications/IntelliJ IDEA CE.app/Contents/MacOS/idea
    0 99841  0  36   0,0 /usr/libexec/amfid
    0 99845  0  31   0,0 /usr/libexec/logd_helper
    0 99852  0   4   0,0 /System/Library/PrivateFrameworks/XprotectFramework.framework/Versions/A/XPCServices/XprotectService.xpc/Contents/MacOS/XprotectService
  501 99854  0   4   0,0 /System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/CarbonCore.framework/Versions/A/XPCServices/csnameddatad.xpc/Contents/MacOS/csnameddatad
  501 99855  0   4   0,0 /System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/HIServices.framework/Versions/A/XPCServices/com.apple.hiservices-xpcservice.xpc/Contents/MacOS/com.apple.hiservices-xpcservice
  501 99862  0  31   0,0 /Applications/IntelliJ IDEA CE.app/Contents/bin/fsnotifier
  501 99870  0   4   0,0 /System/Library/CoreServices/iconservicesagent
  240 99871  0   4   0,0 /System/Library/CoreServices/iconservicesd
    0 99897  0   4   0,0 /System/Library/PrivateFrameworks/TCC.framework/Support/tccd
  501 99933  0   4   0,0 /usr/libexec/transparencyd
  501 99934  0   4   0,0 /System/Library/PrivateFrameworks/DataAccess.framework/Support/dataaccessd
    0 99935  0   4   0,0 /usr/libexec/mobileactivationd
  501 99936  0   4   0,0 /System/Library/Frameworks/CryptoTokenKit.framework/ctkd
  259 99937  0   4   0,0 /System/Library/Frameworks/CryptoTokenKit.framework/ctkd
  501 99938  0   4   0,0 /System/Library/Frameworks/LocalAuthentication.framework/Support/coreauthd
  501 99960  0  37   0,0 /System/Library/CoreServices/TextInputSwitcher.app/Contents/MacOS/TextInputSwitcher
  501 11425  0  31   0,0 /Users/evgenijsudarskij/IdeaProjects/HaskellParBenchmark/EvalBenchmark/.stack-work/install/aarch64-osx/8f0aac3aa2c59a53c22ef08dc54e1ef7917058c4179d1d4640439d210c2dcf5c/9.2.7/bin/EvalBenchmark-exe
  501 12871  0  31   0,0 /Users/evgenijsudarskij/IdeaProjects/HaskellParBenchmark/EvalBenchmark/.stack-work/install/aarch64-osx/8f0aac3aa2c59a53c22ef08dc54e1ef7917058c4179d1d4640439d210c2dcf5c/9.2.7/bin/EvalBenchmark-exe
  501 14016  0  31   0,0 /Users/evgenijsudarskij/IdeaProjects/HaskellParBenchmark/EvalBenchmark/.stack-work/install/aarch64-osx/8f0aac3aa2c59a53c22ef08dc54e1ef7917058c4179d1d4640439d210c2dcf5c/9.2.7/bin/EvalBenchmark-exe
  501 39728  0  31 103,7 /Users/evgenijsudarskij/IdeaProjects/HaskellParBenchmark/EvalBenchmark/.stack-work/install/aarch64-osx/8f0aac3aa2c59a53c22ef08dc54e1ef7917058c4179d1d4640439d210c2dcf5c/9.2.7/bin/EvalBenchmark-exe
    0 80520  0  31   0,0 login
  501 80522  0  31   0,0 -zsh
    0 80719  0  31   0,0 login
  501 80720  0  31   0,0 -zsh
    0 80721  0  31   0,0 login
  501 80724  0  31   0,0 -zsh
    0 24507  0  31   0,0 login
  501 24510  0  31   0,0 -zsh
    0 39763  0  31   0,0 ps