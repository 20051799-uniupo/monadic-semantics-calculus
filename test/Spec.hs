import Core
import Language
import Effects.Nondeterminism

main :: IO ()
main = do
    let e = ExpConf (Magic Choose [])
    let r = reduceStep e :: [Conf NDSig]
    putStrLn $ show r
