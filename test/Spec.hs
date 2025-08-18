import Core
import Data.Map (fromList)
import Effects.Exceptions
import Effects.Nondeterminism
import Language

main :: IO ()
main = do
    putStrLn "--- Example 18 ---"
    let predFun = \x -> Do "p" (IsZero x) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred $ x))
    let e1 = ExpConf $ predFun $ NumVal $ Succ Zero
    putStrLn "- predfun succ 0:"
    putStrLn $ show e1 ++ " =>* " ++ show (eval e1 :: (Either String (Res (ExceptionSig String))))
    let m_e1 = reduceStep e1 :: (Either String (Conf (ExceptionSig String)))
    let m_e1' = m_e1 >>= reduceStep
    let m_e1'' = m_e1' >>= reduceStep
    let m_1''' = m_e1'' >>= reduceStep
    let m_e1'''' = m_1''' >>= reduceStep
    putStrLn $
        show e1
            ++ " ->\n"
            ++ show m_e1
            ++ " =>\n"
            ++ show m_e1'
            ++ " =>\n"
            ++ show m_e1''
            ++ " =>\n"
            ++ show m_1'''
            ++ " =>\n"
            ++ show m_e1''''

    let e2 = ExpConf $ predFun $ NumVal Zero
    putStrLn "- predfun 0:"
    putStrLn $ show e2 ++ " =>* " ++ show (eval e2 :: (Either String (Res (ExceptionSig String))))
    let m_e2 = reduceStep e2 :: (Either String (Conf (ExceptionSig String)))
    let m_e2' = m_e2 >>= reduceStep
    let m_e2'' = m_e2' >>= reduceStep
    let m_e2''' = m_e2'' >>= reduceStep
    putStrLn $
        show e1
            ++ " ->\n"
            ++ show m_e2
            ++ " =>\n"
            ++ show m_e2'
            ++ " =>\n"
            ++ show m_e2''
            ++ " =>\n"
            ++ show m_e2'''

    putStrLn "--- Example 19 ---"
    putStrLn "- e:"
    let e3 = ExpConf $ Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret $ NumVal Zero) (Ret $ NumVal $ Succ Zero))
    putStrLn $ show e3 ++ " =>* " ++ show (eval e3 :: [Res NDSig])
    let m_e3 = reduceStep e3 :: [Conf NDSig]
    let m_e3' = m_e3 >>= reduceStep
    let m_e3'' = m_e3' >>= reduceStep
    let m_e3''' = m_e3'' >>= reduceStep
    putStrLn $
        show e3
            ++ " ->\n"
            ++ show m_e3
            ++ " =>\n"
            ++ show m_e3'
            ++ " =>\n"
            ++ show m_e3''
            ++ " =>\n"
            ++ show m_e3'''

    putStrLn "- chfun 0:"
    let chfun =
            RecLamVal
                "f"
                "x"
                ( Do
                    "y"
                    (Magic Choose [])
                    ( If
                        (IdentifierVal "y")
                        (Ret $ IdentifierVal "x")
                        ( Do
                            "s"
                            (Suc (IdentifierVal "x"))
                            (App (IdentifierVal "f") (IdentifierVal "s"))
                        )
                    )
                )
    let e4 = ExpConf $ App chfun (NumVal Zero)
    let m_e4 = reduceStep e4 :: [Conf NDSig]
    let m_e4' = m_e4 >>= reduceStep
    let m_e4'' = m_e4' >>= reduceStep
    let m_e4''' = m_e4'' >>= reduceStep
    let m_e4'''' = m_e4''' >>= reduceStep
    let m_e4''''' = m_e4'''' >>= reduceStep
    let m_e4'''''' = m_e4''''' >>= reduceStep
    let m_e4''''''' = m_e4'''''' >>= reduceStep
    let m_e4'''''''' = m_e4''''''' >>= reduceStep
    let m_e4''''''''' = m_e4'''''''' >>= reduceStep
    let m_e4'''''''''' = m_e4''''''''' >>= reduceStep
    let m_e4''''''''''' = m_e4'''''''''' >>= reduceStep

    putStrLn $
        show e4
            ++ " ->\n"
            ++ show m_e4
            ++ " =>\n"
            ++ show m_e4'
            ++ " =>\n"
            ++ show m_e4''
            ++ " =>\n"
            ++ show m_e4'''
            ++ " =>\n"
            ++ show m_e4''''
            ++ " =>\n"
            ++ show m_e4'''''
            ++ " =>\n"
            ++ show m_e4''''''
            ++ " =>\n"
            ++ show m_e4'''''''
            ++ " =>\n"
            ++ show m_e4''''''''
            ++ " =>\n"
            ++ show m_e4'''''''''
            ++ " =>\n"
            ++ show m_e4''''''''''
            ++ " =>\n"
            ++ show m_e4'''''''''''

    putStrLn "--- Example 46.1 ---"
    let h =
            Handler
                { handlerClauses =
                    fromList
                        [
                            ( Raise "PredZero"
                            , Clause
                                { clauseMode = Stop
                                , clauseParams = []
                                , clauseBody = Ret $ NumVal Zero
                                }
                            )
                        ]
                , handlerFinal = ("x", Ret $ IdentifierVal "x")
                }

    let e5 = ExpConf $ HandleWith (predFun $ NumVal Zero) h
    putStrLn $ show e5 ++ " =>* " ++ show (eval e5 :: (Either String (Res (ExceptionSig String))))

    let h' =
            Handler
                { handlerClauses =
                    fromList
                        [
                            ( Raise "NotPredZero"
                            , Clause
                                { clauseMode = Stop
                                , clauseParams = []
                                , clauseBody = Ret $ NumVal Zero
                                }
                            )
                        ]
                , handlerFinal = ("x", Ret $ IdentifierVal "x")
                }

    let e6 = ExpConf $ HandleWith (predFun $ NumVal Zero) h'
    putStrLn $ show e6 ++ " =>* " ++ show (eval e6 :: (Either String (Res (ExceptionSig String))))

    putStrLn "--- Example handlers with 'continue' ---"
    let ndExp = Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret $ NumVal Zero) (Ret $ NumVal $ Succ Zero))

    let h1 =
            Handler
                { handlerClauses =
                    fromList
                        [
                            ( Choose
                            , Clause
                                { clauseMode = Continue
                                , clauseParams = []
                                , clauseBody = Ret (BoolVal True)
                                }
                            )
                        ]
                , handlerFinal = ("x", Ret $ IdentifierVal "x")
                }
    let e7 = ExpConf $ HandleWith ndExp h1
    putStrLn $ show e7 ++ " =>* " ++ show (eval e7 :: [Res NDSig])

    let h2 =
            Handler
                { handlerClauses =
                    fromList
                        [
                            ( Choose
                            , Clause
                                { clauseMode = Continue
                                , clauseParams = []
                                , clauseBody = Ret (BoolVal False)
                                }
                            )
                        ]
                , handlerFinal = ("x", Ret $ IdentifierVal "x")
                }
    let e8 = ExpConf $ HandleWith ndExp h2
    putStrLn $ show e8 ++ " =>* " ++ show (eval e8 :: [Res NDSig])
