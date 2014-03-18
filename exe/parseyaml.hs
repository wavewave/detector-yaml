module Main where

import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
-- 
import YAML.Parser
import System.Environment 

-- | testing with a given file
test :: FilePath -> IO () --  PYaml
test fp = do    
    txt <- TIO.readFile fp 
    print (parseOnly p_yaml txt)

main :: IO ()
main = do 
  args <- getArgs
  let fp = args !! 0  
  test fp

