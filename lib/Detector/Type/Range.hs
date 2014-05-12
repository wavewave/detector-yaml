{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}


module Detector.Type.Range where

import           Data.Functor.Identity
-- import           Data.Monoid ((<>), mempty)
import           Data.Scientific
import           Data.Text (Text)
--
import           YAML.Builder
--
import           Detector.Type.Common
-- import           Detector.Type.PTEtaData
 

data RangeDescription m =
  RangeDescription { ranges :: m Range }

deriving instance Show (RangeDescription (Either Import))
deriving instance Show (RangeDescription Identity)
			    
instance MakeYaml (RangeDescription ImportList) where
  makeYaml n = YIArray  . fmap (importOrEmbed n) . unImportList . ranges

instance MakeYaml (RangeDescription  []) where
  makeYaml n = YIArray . fmap (makeYaml n) . ranges

data Range = Range { rangeName :: Text
                   , rangePT   :: Maybe (Scientific,Scientific)
                   , rangeEta  :: Maybe (Scientific,Scientific)
                   }
                   deriving (Show)

instance Nameable Range where
  name = rangeName

instance MakeYaml Range where
  makeYaml n Range {..} = 
    YObject $ [ ("Name"    , mkString (n+defIndent) rangeName) 
              , ("PTRange" , case rangePT of
                               Nothing -> mkString (n+defIndent) "Full"
                               Just (pt1,pt2) -> YLArray Inline [YPrim (YNumber pt1), YPrim (YNumber pt2 )])
              , ("EtaRange", case rangeEta of
                               Nothing -> mkString (n+defIndent) "Full"
                               Just (et1,et2) -> YLArray Inline [YPrim (YNumber et1), YPrim (YNumber et2)]) ]

