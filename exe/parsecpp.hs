{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.String
import System.Environment 
--
import           Test

main :: IO ()
main = do 
  args <- getArgs
  cstr <- newCString (args !! 0)
  c_testffi cstr
