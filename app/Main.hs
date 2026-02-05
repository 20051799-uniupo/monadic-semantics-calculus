module Main (main) where

import Control.Monad (when)
import Core (Conf (ExpConf), evalFin, reduceMnStep)
import Data.Proxy (Proxy (..))
import Options.Applicative (Parser, ParserInfo, execParser, help, helper, info, long, metavar, optional, strArgument, switch, (<**>))
import Parser (ParsedProgram (ParsedProgram), pWhole)
import Text.Megaparsec (errorBundlePretty, parse)
import TypeCheck (typeOf)
import GHC.IO.Handle (hSetEcho)
import GHC.IO.Handle.FD (stdin)

data Opts = Opts
    { inputFile :: Maybe FilePath
    , step :: Bool
    , printType :: Bool
    }

main :: IO ()
main = do
    (opts :: Opts) <- execParser optsInfo

    code <- case (inputFile opts) of
        Just f -> readFile f
        Nothing -> getContents

    case parse pWhole "" code of
        Left err -> putStrLn (errorBundlePretty err)
        Right (ParsedProgram (_ :: Proxy m) ast) -> do
            putStrLn $ "AST: " ++ show ast
            case typeOf ast of
                Left err -> putStrLn $ "Typing error: " ++ show err
                Right t -> do
                    when (printType opts) $ putStrLn $ "Type: " ++ show t
                    let evalStep m_conf = do
                            putStrLn $ "Step: " ++ show m_conf
                            _ <- getLine
                            evalStep (reduceMnStep @m m_conf)
                    if (step opts)
                        then do
                            hSetEcho stdin False
                            evalStep (pure @m (ExpConf ast))
                        else putStrLn $ "Result: " ++ (show $ evalFin @m ast)
  where
    optsInfo :: ParserInfo Opts
    optsInfo = info (optsParser <**> helper) mempty
    optsParser :: Parser Opts
    optsParser =
        Opts
            <$> optional
                (strArgument (metavar "FILE" <> help "Source file"))
            <*> switch (long "step" <> help "Interactively reduce expression")
            <*> switch (long "print-type" <> help "Print expression type-and-effect")
