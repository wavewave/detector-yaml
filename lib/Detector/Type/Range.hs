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
                   , rangePT   :: (Scientific,Scientific)
                   , rangeEta  :: (Scientific,Scientific)
                   }
                   deriving (Show)

instance Nameable Range where
  name = rangeName

instance MakeYaml Range where
  makeYaml n Range {..} = 
    YObject $ [ ("Name"    , mkString (n+defIndent) rangeName) 
              , ("PTRange" , YLArray Inline [YPrim (YNumber pt1) , YPrim (YNumber pt2 )])
              , ("EtaRange", YLArray Inline [YPrim (YNumber eta1), YPrim (YNumber eta2)]) ]
    where (pt1,pt2) = rangePT
          (eta1,eta2) = rangeEta

