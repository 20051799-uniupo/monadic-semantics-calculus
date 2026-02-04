module Main (main) where

import Parser (pWhole, ParsedProgram (ParsedProgram))
import Text.Megaparsec (errorBundlePretty, parse)
import TypeCheck (typeOf)
import Data.Proxy (Proxy(..))
import Core (evalFin)

main :: IO ()
main = do
    -- let code = "using Nondeterminism @ List { handle do x = choose(); return x with { choose() ->c return true, x -> return x } }"
    -- let code = "using Nondeterminism @ List { do x = return lambda x: (Nat ->{} Nat). return x; return x }"
    -- let code = "using Nondeterminism @ List { \
    --     \ do f = return (rec f: Nat -> {} Nat. n. \
    --     \    do t = iszero n; \
    --     \    if t then return 0 \
    --     \    else do n1 = pred n; \
    --     \         do t1 = iszero n1; \
    --     \         if t1 then return n \
    --     \         else do v1 = f n1; \
    --     \              do n2 = pred n1; \
    --     \              do v2 = f n2; \
    --     \              v1 + v2); \
    --     \ do n1 = succ 0; \
    --     \ do n2 = succ n1; \
    --     \ f n2 \
    --     \ }"
    -- let code = "using Nondeterminism @ List { returned }"
    -- let code = "using Nondeterminism @ List { 0 + 0 }"
    -- let code = "using Nondeterminism @ List { return (λx: Nat. return x) }"
    let code = "using Nondeterminism @ List { return 1 }"

    case parse pWhole "" code of
        Left err -> putStrLn (errorBundlePretty err)
        Right (ParsedProgram (_ :: Proxy m) ast) -> do
            putStrLn $ "AST: " ++ show ast
            case typeOf ast of
                Left err -> putStrLn $ "Typing error: " ++ show err
                Right t -> do
                    putStrLn $ "Type: " ++ show t
                    putStrLn $ "Result: " ++ (show $ evalFin @m ast)
