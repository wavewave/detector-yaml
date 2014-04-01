{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Detector.Type.Common where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor.Identity
-- import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text.Lazy as L
import           Data.Traversable
-- 
import           YAML.Builder

deriving instance Foldable (Either e)

deriving instance Traversable (Either e)

instance (Show a) => Show (Identity a) where
  show x = show (runIdentity x)

class Nameable a where
  name :: a -> Text

data Import = Import { fileName :: Text }
            deriving (Show)

newtype ImportList a = ImportList { unImportList :: [Either Import a] }

deriving instance Functor ImportList

data MetaInfo = MetaInfo { tag :: Text
                         , description :: Text
                         , comment :: Text 
                         , reference :: Text } 

              deriving (Show)

mkImport :: Int -> Import -> YamlValue
mkImport n Import {..} = 
  YObject $ [ ("Import", mkString (n+defIndent) fileName) ] 

mkMetaInfoPairs :: Int -> MetaInfo -> [ (L.Text, YamlValue) ]
mkMetaInfoPairs n MetaInfo {..} = 
  [ ("Tag" , mkString n tag)
  , ("Description", mkString n description) 
  , ("Comment", mkString n comment )
  , ("Reference", mkString n reference ) ]


importOrEmbed :: (MakeYaml a) => 
                 Int 
              -> Either Import a 
              -> YamlValue
importOrEmbed n = either (mkImport n) (makeYaml n)

importOrEmbedF :: (MakeYaml a, Applicative f) => 
                 Int 
              -> Either Import (f a) 
              -> f YamlValue
importOrEmbedF n = fmap (importOrEmbed n) . sequenceA

