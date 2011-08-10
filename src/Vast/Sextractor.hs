{-# LANGUAGE TypeSynonymInstances #-}
module Vast.Sextractor ( solve
                       , solveU
                       , mkOption
                       , opts
                       , opts1 
                       , Options (..)) where

import System.Cmd (rawSystem)
import IO
import System.Plugins.Utils (mkTemp)
import System.Directory (removeFile)
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import Control.Parallel

data Options = Options { sex    :: Sex
                       , params :: [Param] 
                       } deriving (Show)
                                  
type ReadFile = FilePath
type WriteFile = FilePath
type Sex = [String]
type Param = String

class ShowO a where
  showO :: a -> String

instance ShowO String where
  showO = id
 
instance ShowO Int where
  showO = show
  
instance ShowO Double where
  showO = show

printParams :: [Param] -> String
printParams = concatMap (++"\n") 

mkOption :: (ShowO a) => String -> a -> Sex
mkOption s a = ['-':s, showO a]

opts :: Options
opts = Options { sex = []
               , params = [ "X_IMAGE"
                          , "Y_IMAGE"
                          , "FLUX_RADIUS"
                          , "FLUX_MAX"
                          , "FLUX_APER(1)"
                          , "FLUXERR_APER(1)"
                          , "ELONGATION"
                          , "FLAGS" ] }

opts1 :: Options
opts1 = Options { sex = []
                , params = [ "X_IMAGE"
                           , "Y_IMAGE"
                           , "FLAGS" ] }
       
solveU :: Options -> ReadFile -> String
solveU o = unsafePerformIO . solve o

solve :: Options -> ReadFile -> IO (String)
solve o rf = makeTemp $ \file -> do
  runSextractor o rf file
  readFile file
  
makeTemp :: (FilePath -> IO a) -> IO a
makeTemp g = bracket mkTemp (removeFile . fst) $ 
             \(f, h) -> hClose h >> g f
        
runSextractor :: Options -> ReadFile -> WriteFile -> IO ()
runSextractor o r w = makeTemp $ \p -> do
  writeFile p $ printParams (params o)
  runSex (sex o 
          ++ mkOption "PARAMETERS_NAME" p
          ++ mkOption "CATALOG_NAME" w) r

runSex :: Sex -> ReadFile -> IO ()
runSex opts f = rawSystem "sex" (mkOption "c" "default.sex" ++ opts ++ [f]) 
                >> return ()

