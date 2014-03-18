{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Detector.Parser where

import qualified Data.List as L
--
import Detector.Type
-- 
import YAML.Parser

findName :: PYaml -> Maybe PYaml
findName = L.lookup "name" 

