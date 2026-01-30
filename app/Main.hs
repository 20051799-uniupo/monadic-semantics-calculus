module Main (main) where

import Parser (pWhole, ParsedProgram (ParsedProgram))
import Text.Megaparsec (errorBundlePretty, parse)
import TypeCheck (typeOf)
import Data.Proxy (Proxy(..))
import Core (evalFin)

main :: IO ()
main = do
    -- let code = "using Nondeterminism @ List { handle do x = choose(); return x with { choose() ->c return true, x -> return x } }"
    let code = "using Nondeterminism @ List { do x = return lambda x: Nat. return x; x(0) }"

    case parse pWhole "" code of
        Left err -> putStrLn (errorBundlePretty err)
        Right (ParsedProgram (_ :: Proxy m) ast) -> do
            putStrLn $ "AST: " ++ show ast
            case typeOf ast of
                Left err -> putStrLn $ "Typing error: " ++ show err
                Right t -> do
                    putStrLn $ "Type: " ++ show t
                    putStrLn $ "Result: " ++ (show $ evalFin @m ast)
