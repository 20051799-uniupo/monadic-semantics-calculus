import Core
import Effects.Exceptions
import Effects.Nondeterminism
import Language

main :: IO ()
main = do
  putStrLn "---Example 18---"
  let predFun = \x -> ExpConf $ Do "p" (IsZero x) (If (IdentifierVal "p") (Magic (Raise "PredZero") []) (Pred $ x))
  let e1 = predFun $ NumVal $ Succ Zero
  putStr "- predfun succ 0: "
  putStrLn $ show e1 ++ " =>* " ++ show (eval e1 :: (Either String (Res (ExceptionSig String))))
  let m_e1 = reduceStep e1 :: (Either String (Conf (ExceptionSig String)))
  let m_e1' = m_e1 >>= reduceStep
  let m_e1'' = m_e1' >>= reduceStep
  let m_1''' = m_e1'' >>= reduceStep
  let m_e1'''' = m_1''' >>= reduceStep
  putStrLn $
    show e1 ++ " -> " ++ show m_e1 ++ " => " ++ show m_e1' ++ " => " ++ show m_e1'' ++ " => " ++ show m_1''' ++ " => " ++ show m_e1''''

  let e2 = predFun $ NumVal Zero
  putStr "- predfun 0: "
  putStrLn $ show e2 ++ " =>* " ++ show (eval e2 :: (Either String (Res (ExceptionSig String))))
  let m_e2 = reduceStep e2 :: (Either String (Conf (ExceptionSig String)))
  let m_e2' = m_e2 >>= reduceStep
  let m_e2'' = m_e2' >>= reduceStep
  let m_e2''' = m_e2'' >>= reduceStep
  let m_e2'''' = m_e2''' >>= reduceStep
  putStrLn $
    show e1 ++ " -> " ++ show m_e2 ++ " => " ++ show m_e2' ++ " => " ++ show m_e2'' ++ " => " ++ show m_e2''' ++ " => " ++ show m_e2''''

  putStrLn "---Example 19---"
  let e3 = ExpConf $ Do "y" (Magic Choose []) (If (IdentifierVal "y") (Ret $ NumVal Zero) (Ret $ NumVal $ Succ Zero))
  let m_r = eval e3 :: [Res NDSig]
  putStrLn $ show e3 ++ " =>* " ++ show m_r

  let m_e3 = reduceStep e3 :: [Conf NDSig]
  let m_e3' = m_e3 >>= reduceStep
  let m_e3'' = m_e3' >>= reduceStep
  let m_e3''' = m_e3'' >>= reduceStep

  putStrLn $
    show e3 ++ " -> " ++ show m_e3 ++ " => " ++ show m_e3' ++ " => " ++ show m_e3'' ++ " => " ++ show m_e3'''
