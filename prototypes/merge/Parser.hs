module Parser (fParse) where

import Data.Attoparsec.ByteString.Char8 (sepBy, double, skipWhile, isSpace, parseOnly, char)
import qualified Data.ByteString as B
import Control.Monad

parser = line `sepBy` char '\n' where
  line = do
    x <- double
    skipWhile isSpace
    y <- double
    skipWhile isSpace
    double
    return (x,y)
  
fParse :: FilePath -> IO [(Double, Double)]
fParse name = do
  raw <- B.readFile name
  case parseOnly parser raw of
    Left e  -> error $ show e
    Right xs -> return xs 
  
