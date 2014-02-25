{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- import           Blaze.ByteString.Builder
-- import           Blaze.ByteString.Builder.Char8
import           Data.Text.Lazy.Builder 
import           Data.Attoparsec
import qualified Data.ByteString.Lazy.Char8 as L
-- import           Data.Foldable
-- import qualified Data.HashMap.Strict as HM
import           Data.List (intersperse)
import           Data.Monoid ((<>), mconcat, mempty)
import           Data.Scientific
import qualified Data.String as S (IsString(..), fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V

data ListStyle = Inline | Wrapped 

data YamlValue = YObject [(T.Text, YamlValue)]
               | YLArray ListStyle [YamlValue]
               --  | YIArray [YamlValue]
               | YPrim YamlPrimValue 

data YamlPrimValue = YNumber Scientific
                   | YInteger Int 
                   | YString T.Text
                   | YBool Bool 
                   | YNull 

isPrim :: YamlValue -> Bool 
isPrim (YPrim _) = True
isPrim _ = False 

instance S.IsString YamlPrimValue where
  fromString str = YString (T.pack str) 

instance S.IsString YamlValue where
  fromString = YPrim . S.fromString

defIndent :: Int
defIndent = 4 

buildYaml :: Int -> YamlValue -> Builder 
buildYaml n (YObject m) = (mconcat . map (buildPair n) ) m
buildYaml n (YLArray sty xs) = buildList sty n xs 
buildYaml n (YPrim p) = buildPrim p 

buildList :: ListStyle -> Int -> [YamlValue] -> Builder 
buildList Inline n xs = fromLazyText "[ "
                        <> (mconcat . intersperse (fromLazyText ", ") . map (buildYaml n)) xs 
                        <> fromLazyText " ]"
buildList Wrapped n xs = fromLazyText "\n" <> makeIndent n 
                         <> fromLazyText "[ " 
                         <> ( mconcat 
                            . intersperse (makeIndent n <> fromLazyText ", ") 
                            . map (\x -> buildYaml n x <> fromLazyText "\n") ) xs
                         <> makeIndent n <> fromLazyText "] "

buildPrim (YNumber s) = scientificBuilder s 
buildPrim (YInteger s) = (fromLazyText . T.pack . show) s
buildPrim (YString txt) = fromLazyText txt
buildPrim (YBool b) = (fromLazyText . T.pack . show) b 
buildPrim YNull = mempty 



buildPair :: Int -> (T.Text, YamlValue) -> Builder
buildPair n (k,v) = fromLazyText "\n" <> makeIndent n 
                    <> fromLazyText k 
                    <> fromLazyText ": "
                    <> buildYaml (n+defIndent) v

makeIndent :: Int -> Builder 
makeIndent n = mconcat (replicate n (fromString "_"))

-- eleptbins = YLArray Inline $ map (YPrim . YNumber) [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0] 

-- eleetabins = YLArray Inline $ map (YPrim . YNumber) [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
 
data ElectronEfficiency = ElectronEfficiency 
			    { elePtBins :: [Scientific] 
			    , eleEtaBins :: [Scientific] 
			    , nEleEta :: Int
			    , nElePt :: Int 
			    , tightEleEff :: [ [ Scientific ] ]
			    , mediumEleEff :: [ [ Scientific ] ] 
			    , looseEleEff :: [ [ Scientific ] ] 
			    }

data ATLASInfo = ATLASInfo { elecEff :: ElectronEfficiency 

-- elePtBins :: [Scientific] 
--                            , eleEtaBins :: [Scientific] 
--                            , nEleEta :: Int
--                            , nElePt :: Int 
--                            , tightEleEff :: [ [ Scientific ] ]
--                           , mediumEleEff :: [ [ Scientific ] ] 
--                           , looseEleEff :: [ [ Scientific ] ] 
                           }


mkInline :: [Scientific] -> YamlValue 
mkInline = YLArray Inline . map (YPrim . YNumber)  

mkWrap :: [YamlValue] -> YamlValue 
mkWrap = YLArray Wrapped 

mkElectronEfficiency :: ElectronEfficiency -> YamlValue
mkElectronEfficiency ElectronEfficiency {..} = 
    YObject $ [ ("ElePtBins", mkInline elePtBins)
            , ("EleEtaBins", mkInline eleEtaBins)
            , ("nEleEta", (YPrim . YInteger) nEleEta) 
            , ("nElePt" , (YPrim . YInteger) nElePt)
            , ("TightEleEff", mkWrap (map mkInline tightEleEff) ) 
            , ("MediumEleEff", mkWrap (map mkInline mediumEleEff) )
            , ("LooseEleEff", mkWrap (map mkInline looseEleEff) )
            ]


mkATLAS :: ATLASInfo -> YamlValue 
mkATLAS ATLASInfo {..} = 
    YObject $ [ ("ElectronEfficiency", mkElectronEfficiency elecEff) ] 


testvalue = mkATLAS (ATLASInfo { elecEff = atlasElecEff })

atlasElecEff :: ElectronEfficiency
atlasElecEff = ElectronEfficiency 
  { elePtBins = [4.0, 7.0, 10.0, 15.0, 20.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0, 80.0]
  , eleEtaBins = [-2.5, -2.0, -1.52, -1.37, -0.75, 0.0, 0.75, 1.37, 1.52, 2.0, 2.5]
  , nEleEta = 10
  , nElePt = 12 
  , tightEleEff = 
      [ [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ]
      , [ 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8 ] ]
  , mediumEleEff = 
      [ [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ]
      , [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.98, 0.98, 0.98, 0.98 ] ]
  , looseEleEff = 
      [ [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ]
      , [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] ]

  }
 

testvalue2 = YObject $ [ ("hello", "okay") 
                       , ("world", "no")
                       ] 

testvalue3 = YLArray Wrapped [ v1, v2 , "abc", "babo" ] 

v1 = YLArray Inline $ map (YPrim . YNumber) [1, 2, 3 ] 

v2 = YLArray Inline $ map (YPrim . YNumber) [ 4, 5, 6 ] 

main :: IO ()
main = do 
  putStrLn "yayaml"
  TIO.putStrLn $ toLazyText (buildYaml 4 testvalue)
