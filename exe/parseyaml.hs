module Main where

import YAML.Parser
import System.Environment 

main :: IO ()
main = do 
  args <- getArgs
  let fp = args !! 0  
  test fp
