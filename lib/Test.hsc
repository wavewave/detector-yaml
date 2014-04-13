{-# LANGUAGE ForeignFunctionInterface #-}

module Test where

import Foreign.C.String

foreign import ccall "testffi.h testffi" c_testffi 
  :: CString -> IO ()
