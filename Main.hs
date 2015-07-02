module Main where
import Attempto.Lexicon
import Data.ByteString (readFile)
import Prelude hiding (readFile)
import System.IO (hFlush, stdout)

lexFile :: FilePath
lexFile = "clex_lexicon.pl"

main :: IO ()
main = undefined
