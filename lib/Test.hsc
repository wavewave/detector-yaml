{-# LANGUAGE ForeignFunctionInterface #-}

module Test where

foreign import ccall "testffi.h testffi" c_testffi 
  :: IO ()
