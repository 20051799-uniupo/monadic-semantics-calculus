-- TODO: use proper test framework
import Core
import Data.Map (fromList)
import Effects.Exceptions
import Effects.Nondeterminism
import Language
import Nat (Nat (..), Peano)
import TypeCheck (typeOf)

main :: IO ()
main = do
    putStrLn "--- Example 18 ---"
    let predFun = \x -> Do "p" (IsZero x) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred $ x))
    let e1 = predFun $ NatVal $ succ (zero :: Peano)
    putStrLn "- predfun succ 0:"
    putStrLn $ show e1 ++ " =>* " ++ show (evalFin e1 :: (Either String (Res (ExceptionSig String))))
    let m_e1 = reduceStep (ExpConf e1) :: (Either String (Conf (ExceptionSig String)))
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

    let e2 = predFun $ NatVal (zero :: Peano)
    putStrLn "- predfun 0:"
    putStrLn $ show e2 ++ " =>* " ++ show (evalFin e2 :: (Either String (Res (ExceptionSig String))))
    let m_e2 = reduceStep (ExpConf e2) :: (Either String (Conf (ExceptionSig String)))
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
    let e3 = Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret $ NatVal (zero :: Peano)) (Ret $ NatVal $ succ (zero :: Peano)))
    putStrLn $ show e3 ++ " =>* " ++ show (evalFin e3 :: [Res NDSig])
    let m_e3 = reduceStep (ExpConf e3) :: [Conf NDSig]
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
    let e4 = ExpConf $ App chfun (NatVal (zero :: Peano))
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
                                , clauseBody = Ret $ NatVal (zero :: Peano)
                                }
                            )
                        ]
                , handlerFinal = ("x", Ret $ IdentifierVal "x")
                }

    let e5 = HandleWith (predFun $ NatVal (zero :: Peano)) h
    putStrLn $ show e5 ++ " =>* " ++ show (evalFin e5 :: (Either String (Res (ExceptionSig String))))

    let h' =
            Handler
                { handlerClauses =
                    fromList
                        [
                            ( Raise "NotPredZero"
                            , Clause
                                { clauseMode = Stop
                                , clauseParams = []
                                , clauseBody = Ret $ NatVal (zero :: Peano)
                                }
                            )
                        ]
                , handlerFinal = ("x", Ret $ IdentifierVal "x")
                }

    let e6 = HandleWith (predFun $ NatVal (zero :: Peano)) h'
    putStrLn $ show e6 ++ " =>* " ++ show (evalFin e6 :: (Either String (Res (ExceptionSig String))))

    putStrLn "--- Example handlers with 'continue' ---"
    let ndExp = Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret $ NatVal (zero :: Peano)) (Ret $ NatVal $ succ (zero :: Peano)))

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
    let e7 = HandleWith ndExp h1
    putStrLn $ show e7 ++ " =>* " ++ show (evalFin e7 :: [Res NDSig])

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
    let e8 = HandleWith ndExp h2
    putStrLn $ show e8 ++ " =>* " ++ show (evalFin e8 :: [Res NDSig])

    putStrLn "--- Eaxmple typechecking ---"
    let e9 = Ret $ LamVal "x" (Plus (IdentifierVal "x") (NatVal (zero :: Peano))) :: Exp NDSig
    let t = typeOf e9
    putStrLn $ "typeOf " ++ show e9 ++ " = " ++ show t

    let e10 = App (LamVal "y" (App (LamVal "x" (Ret (IdentifierVal "x"))) (IdentifierVal "y"))) (NatVal (zero :: Peano)) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e10 ++ " = " ++ show t

    let e11 = App (LamVal "x" (Ret (IdentifierVal "x"))) (NatVal (zero :: Peano)) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e11 ++ " = " ++ show t

    let e12 = Ret (LamVal "x" (Ret $ IdentifierVal "x")) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e12 ++ " = " ++ show t

    let e13 = Ret (LamVal "x" (Ret $ BoolVal True)) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e13 ++ " = " ++ show t

    let e14 = Ret (LamVal "x" (App (LamVal "x" (Plus (IdentifierVal "x") (NatVal (zero :: Peano)))) (IdentifierVal "x"))) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e14 ++ " = " ++ show t

    let e15 = Ret (LamVal "x" (App (LamVal "x" (IsZero (IdentifierVal "x"))) (IdentifierVal "x"))) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e15 ++ " = " ++ show t

    let e16 = App (LamVal "id" (App (LamVal "f" (Ret (IdentifierVal "f"))) (IdentifierVal "id"))) (LamVal "y" (Ret (IdentifierVal "y"))) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e16 ++ " = " ++ show t

    let e17 = App (LamVal "x" (Ret (IdentifierVal "x"))) (LamVal "y" (Ret (IdentifierVal "y"))) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e17 ++ " = " ++ show t

    let e18 = App (LamVal "f" (App (IdentifierVal "f") (NatVal (zero :: Peano)))) (LamVal "x" (Ret (IdentifierVal "x"))) :: Exp NDSig
    putStrLn $ "typeOf " ++ show e18 ++ " = " ++ show t

    pure ()
