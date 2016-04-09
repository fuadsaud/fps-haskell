reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConstFalse))) (CVar "X")) (CVar "Y"))
reduceComb (CAppl (CAppl (Ccon primPlus) (Ccon (IConst (-2)))) (Ccon (IConst 5)))




compileAndRun (Appl (Lambda "x" (Econ (IConst 5))) (Appl (Appl (Var "div") (Econ (IConst 1))) (Econ (IConst 0))))


compileAndRun (Appl
                (Lambda "x"
                        (IfExp (Appl
                                 (Appl (Var "==") (Var "x"))
                                 (Econ (IConst 0)))
                               (Var "x")
                               (Appl
                                 (Appl (Var "+") (Var "x"))
                                 (Econ (Iconst 1)))))
                (Econ (IConst 3)))

compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var "==") (Var "x")) (Econ (IConst 0))) (Var "x") (Appl (Appl (Var "+") (Var "x")) (Econ (IConst 1))))) (Econ (IConst 3)))

compileAndRun (Appl (Lambda "x" (IfExp (Appl (Appl (Var"==") (Var "x")) (Econ (IConst 0))) (Var "x") (Appl (Appl (Var "+")(Var "x")) (Econ (IConst 1))))) (Econ (IConst 0)))
