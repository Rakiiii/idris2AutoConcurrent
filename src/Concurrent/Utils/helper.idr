






-- Bad elaborator script foldr ((*>) . declare) (pure ()) (fst (either (Delay (\_ => ([], []))) (Delay id) (rxJoin (rxMap (rxMapInternalFst (rxMapInternalFst (rxMapInternalFst
--  (rxMapInternalFst (parseLambdaFunction (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 27) (252, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 32) (70, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 41) (250, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 32) (64, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 64) (250, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 54) (252, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) ?delayed) 
-- constructDataDependencieGraph) (dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1))) (\lamc => let (graph, partition) = lamc in let functionsBodies = mapT generateFunctionBody partition in (graph, (partition, functionsBodies)))) (\lamc => let ((graph, (partition, bodies)), argType) = lamc in composeFunctions (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (217, 61) (217, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 27) (252, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 32) (70, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (70, 21) (70, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 7) (252, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 30) (252, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 32) (67, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (67, 21) (67, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 41) (250, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 32) (64, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (64, 21) (64, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 21) (250, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 44) (250, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (58, 39) (58, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 39) (248, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 38) (249, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 32) (61, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (61, 21) (61, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 18) (249, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (248, 58) (248, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 64) (250, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 54) (252, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments"))))))) (fromString "concurrentFunctionBaseName") partition graph bodies)) ()))) (script is not a data value).






-- Bad elaborator script foldr ((*>) . declare) (pure ()) (fst (either (Delay (\_ => ([], []))) (Delay id) (rxJoin (rxMap (rxMapInternalFst (rxMapInternalFst (rxMapInternalFst (rxMapInternalFst (parseLambdaFunction (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 27) (253, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 32) (71, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 21) (71, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 41) (252, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 21) (68, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 32) (65, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 21) (65, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 64) (251, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 54) (253, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) ?delayed) constructDataDependencieGraph) (dup (simplifyDependencies . doBiPartition RandomBiPartitioner WeightAll1))) (\lamc => let (graph, partition) = lamc in let functionsBodies = mapT generateFunctionBody partition in (graph, (partition, functionsBodies)))) (\lamc => let ((graph, (partition, bodies)), argType) = lamc in composeFunctions (ILam (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) MW ExplicitArg (Just (UN (Basic "saw"))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 77)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (218, 61) (218, 75)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "ConcurrentWrap")))) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 27) (253, 29)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 32) (71, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (71, 21) (71, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 7) (253, 26)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction4"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 69)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 30) (253, 37)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 51)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 41) (252, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 32) (68, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (68, 21) (68, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (252, 21) (252, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction3"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 41) (251, 43)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 32) (65, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (65, 21) (65, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 21) (251, 40)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction2"))))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 79)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 44) (251, 51)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "concat2")))) (UN (Basic "c")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (59, 39) (59, 46)) (PrT IntegerType))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 39) (249, 46)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw")))) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (INamedApp (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 44)) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 38) (250, 40)) (NS (MkNS ["ConcurrentDsl", "Api", "Concurrent"]) (UN (Basic "<<")))) (UN (Basic "b")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 32) (62, 39)) (PrT IntegerType))) (UN (Basic "a")) (IPrimVal (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (62, 21) (62, 28)) (PrT IntegerType))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (250, 18) (250, 37)) (NS (MkNS ["Main"]) (UN (Basic "concurrentFunction1"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (249, 58) (249, 61)) (UN (Basic "saw"))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (251, 64) (251, 79)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments")))))))) (IVar (MkFC (PhysicalIdrSrc (MkMI ["Main"])) (253, 54) (253, 69)) (NS (MkNS ["Main"]) (UN (Basic "concatArguments"))))))) (fromString "concurrentFunctionBaseName") partition graph bodies)) ()))) (script is not a data value).
-- idris2

[ [ IClaim
      emptyFC
      MW
      Public
      []
      (mkTy
         { name = "concurrentFunction_concurrent_function_1"
         , type =
                 MkArg
                   MW
                   ExplicitArg
                   Nothing
                   (   var "System.Concurrency.Channel"
                    .$ primVal (PrT IntegerType))
             .-> MkArg
                   MW
                   ExplicitArg
                   Nothing
                   (   var "System.Concurrency.Channel"
                    .$ primVal (PrT IntegerType))
             .-> MkArg
                   MW
                   ExplicitArg
                   Nothing
                   (   var "System.Concurrency.Channel"
                    .$ primVal (PrT IntegerType))
             .-> MkArg
                   MW
                   ExplicitArg
                   Nothing
                   (   var "System.Concurrency.Channel"
                    .$ primVal (PrT IntegerType))
             .-> MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
             .->    var "IO"
                 .$ alternative
                      { tpe = UniqueDefault (var "MkUnit")
                      , alts = [var "Unit", var "MkUnit"]
                      }
         })
  , IDef
      emptyFC
      "concurrentFunction_concurrent_function_1"
      [       var "concurrentFunction_concurrent_function_1"
           .$ bindVar "channel_Main_concurrentFunction2"
           .$ bindVar "channel_Main_concurrentFunction3"
           .$ bindVar "channel_Main_concurrentFunction1"
           .$ bindVar "channel_Main_concurrentFunction4"
           .$ bindVar "saw"
        .= var "id" .$ (var "fromInteger" .$ primVal (BI 10))
      ]
  ]
, [ IClaim
      emptyFC
      MW
      Public
      []
      (mkTy
         { name = "concurrentFunction_concurrent_function_2"
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
             .->    var "IO"
                 .$ alternative
                      { tpe = UniqueDefault (var "MkUnit")
                      , alts = [var "Unit", var "MkUnit"]
                      }
         })
  , IDef
      emptyFC
      "concurrentFunction_concurrent_function_2"
      [       var "concurrentFunction_concurrent_function_2"
           .$ bindVar "channel_Main_concurrentFunction2"
           .$ bindVar "channel_Main_concurrentFunction3"
           .$ bindVar "channel_Main_concurrentFunction1"
           .$ bindVar "channel_Main_concurrentFunction4"
           .$ bindVar "saw"
        .= ilet
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
                 .$ ilet
                      { count = MW
                      , name = "Main_concurrentFunction3_result_0"
                      , type = implicitTrue
                      , val =
                             var "Main.concurrentFunction3"
                          .$ var "Main_concurrentFunction2_result_0"
                      , scope =
                             var "Prelude.Interfaces.(>>)"
                          .$ (   var "System.Concurrency.channelPut"
                              .$ var "channel_Main_concurrentFunction3"
                              .$ var "Main_concurrentFunction3_result_0")
                          .$ ilet
                               { count = MW
                               , name = "Main_concurrentFunction1_result_0"
                               , type = implicitTrue
                               , val = var "Main.concurrentFunction1" .$ var "saw"
                               , scope =
                                      var "Prelude.Interfaces.(>>)"
                                   .$ (   var "System.Concurrency.channelPut"
                                       .$ var "channel_Main_concurrentFunction1"
                                       .$ var "Main_concurrentFunction1_result_0")
                                   .$ (   var "System.Concurrency.channelPut"
                                       .$ var "channel_Main_concurrentFunction4"
                                       .$ (   var "Main.concurrentFunction4"
                                           .$ (   var "Main.concatArguments"
                                               .$ var
                                                    "Main_concurrentFunction1_result_0"
                                               .$ var
                                                    "Main_concurrentFunction3_result_0")))
                               }
                      }
             }
      ]
  ]
]





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
























left: [ 
        Function: "Main.concurrentFunction2" 
            Called with concatanation of: ["saw", "Main_concurrentFunction1_result_0"] 
                concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: "Main.concatArguments"; 
                    Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction2_result_0"]
                        Dependencies: ["Main_concurrentFunction1_result_0"]
                            Call sequence number: 2, 
        
        Function: "Main.concurrentFunction3" 
            Called with wraped variable: "Main_concurrentFunction2_result_0"; 
                Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction3_result_0"]
                    Dependencies: ["Main_concurrentFunction2_result_0"]
                        Call sequence number: 3
      ]
right: [ 
         Function: "Main.concurrentFunction1" 
            Called with wraped variable: "saw"; 
                Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction1_result_0"]
                    Dependencies: []
                        Call sequence number: 1, 
         
         Function: "Main.concurrentFunction4" 
            Called with concatanation of: ["Main_concurrentFunction1_result_0", "Main_concurrentFunction3_result_0"] 
                concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: "Main.concatArguments"; 
                    Returns type IntegerType Result of type IntegerType was not saved
                        Dependencies: ["Main_concurrentFunction3_result_0", "Main_concurrentFunction1_result_0"]
                            Call sequence number: 4
       ]


left: [ 
        Function: "Main.concurrentFunction2" 
            Called with concatanation of: ["saw", "Main_concurrentFunction1_result_0"] 
                concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: "Main.concatArguments"; 
                    Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction2_result_0"]
                        Dependencies: ["Main_concurrentFunction1_result_0"]
                            Call sequence number: 2, 
        
        Function: "Main.concurrentFunction3" 
            Called with wraped variable: "Main_concurrentFunction2_result_0"; 
                Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction3_result_0"]
                    Dependencies: []
                        Call sequence number: 3
      ]
right: [ 
         Function: "Main.concurrentFunction1" 
            Called with wraped variable: "saw"; 
                Returns type IntegerType Result of type IntegerType was saved to: ["Main_concurrentFunction1_result_0"]
                    Dependencies: []
                        Call sequence number: 1, 
         
         Function: "Main.concurrentFunction4" 
            Called with concatanation of: ["Main_concurrentFunction1_result_0", "Main_concurrentFunction3_result_0"] 
                concatenated by function: "ConcurrentDsl.Api.Concurrent.concat1" user composition function: "Main.concatArguments"; 
                    Returns type IntegerType Result of type IntegerType was not saved
                        Dependencies: ["Main_concurrentFunction3_result_0"]
                            Call sequence number: 4
       ]




















( [ [ IClaim
        emptyFC
        MW
        Public
        []
        (mkTy
           { name = "concurrentFunction_concurrent_function_1"
           , type =
                   MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
               .->    var "IO"
                   .$ alternative
                        { tpe = UniqueDefault (var "MkUnit")
                        , alts = [var "Unit", var "MkUnit"]
                        }
           })
    , IDef
        emptyFC
        "concurrentFunction_concurrent_function_1"
        [       var "concurrentFunction_concurrent_function_1"
             .$ bindVar "channel_Main_concurrentFunction2"
             .$ bindVar "channel_Main_concurrentFunction3"
             .$ bindVar "channel_Main_concurrentFunction1"
             .$ bindVar "channel_Main_concurrentFunction4"
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
           { name = "concurrentFunction_concurrent_function_2"
           , type =
                   MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg
                     MW
                     ExplicitArg
                     Nothing
                     (   var "System.Concurrency.Channel"
                      .$ primVal (PrT IntegerType))
               .-> MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
               .->    var "IO"
                   .$ alternative
                        { tpe = UniqueDefault (var "MkUnit")
                        , alts = [var "Unit", var "MkUnit"]
                        }
           })
    , IDef
        emptyFC
        "concurrentFunction_concurrent_function_2"
        [       var "concurrentFunction_concurrent_function_2"
             .$ bindVar "channel_Main_concurrentFunction2"
             .$ bindVar "channel_Main_concurrentFunction3"
             .$ bindVar "channel_Main_concurrentFunction1"
             .$ bindVar "channel_Main_concurrentFunction4"
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
                                       .$ var
                                            "Main_concurrentFunction1_result_0"
                                       .$ var
                                            "Main_concurrentFunction3_result_0"))))
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
         { name = "concurrentFunction"
         , type =
                 MkArg MW ExplicitArg Nothing (primVal (PrT IntegerType))
             .-> var "IO" .$ primVal (PrT IntegerType)
         })
  , IDef
      emptyFC
      "concurrentFunction"
      [    var "concurrentFunction" .$ bindVar "saw"
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
                           .$ (    MkArg
                                     MW
                                     ExplicitArg
                                     (Just "channel_Main_concurrentFunction1")
                                     implicitFalse
                               .=>    var "Prelude.Interfaces.(>>=)"
                                   .$ (   var "System.Concurrency.makeChannel"
                                       .! ("a", primVal (PrT IntegerType))
                                       .! ("io", var "PrimIO.IO"))
                                   .$ (    MkArg
                                             MW
                                             ExplicitArg
                                             (Just "channel_Main_concurrentFunction4")
                                             implicitFalse
                                       .=>    var "Prelude.Interfaces.(>>=)"
                                           .$ (   var "Prelude.IO.fork"
                                               .$ (   var "concurrentFunction_concurrent_function_1"
                                                   .$ var "channel_Main_concurrentFunction2"
                                                   .$ var "channel_Main_concurrentFunction3"
                                                   .$ var "channel_Main_concurrentFunction1"
                                                   .$ var "channel_Main_concurrentFunction4"
                                                   .$ var "saw"))
                                           .$ (    MkArg MW ExplicitArg Nothing (var "PrimIO.ThreadID")
                                               .=>    var "Prelude.Interfaces.(>>=)"
                                                   .$ (   var "Prelude.IO.fork"
                                                       .$ (   var
                                                                "concurrentFunction_concurrent_function_2"
                                                           .$ var "channel_Main_concurrentFunction2"
                                                           .$ var "channel_Main_concurrentFunction3"
                                                           .$ var "channel_Main_concurrentFunction1"
                                                           .$ var "channel_Main_concurrentFunction4"
                                                           .$ var "saw"))
                                                   .$ (    MkArg
                                                             MW
                                                             ExplicitArg
                                                             Nothing
                                                             (var "PrimIO.ThreadID")
                                                       .=>    var "System.Concurrency.channelGet"
                                                           .! ("a", primVal (PrT IntegerType))
                                                           .! ("io", var "PrimIO.IO")
                                                           .$ var "channel_Main_concurrentFunction4"))))))
      ]
  ]
)

