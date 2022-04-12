{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}




import Data.Aeson
import Control.Applicative
import Data.Text
import GHC.Generics
import Text.JSON.Generic
import Data.Typeable
import Network.HTTP
import Data.Map
import Data.Maybe
import Data.Either
import Data.ByteString
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

packStr :: String -> ByteString
packStr input = C.pack input

jsonURL :: String
jsonURL = "http://www.uvek-gis.admin.ch/BFE/ogd/52/Solarenergiepotenziale_Gemeinden_Daecher_und_Fassaden.json"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

jsonURLTest :: String
jsonURLTest = "./test.json"

getJSONLocal :: IO B.ByteString
getJSONLocal = B.readFile jsonURLTest

data Info =
  Info { municipalityNumber  :: Float
         , municipalityName   :: String
         , canton   :: Maybe String
         , country   :: String
         , scenario1_RoofsOnly_PotentialSolarElectricity_GWh :: Float
         , scenario2_RoofsOnly_PotentialSolarElectricity_GWh :: Float
         , scenario2_RoofsOnly_PotentialSolarHeat_GWh :: Float
         , scenario3_RoofsFacades_PotentialSolarElectricity_GWh :: Float
         , scenario4_RoofsFacades_PotentialSolarElectricity_GWh :: Float
         , scenario4_RoofsFacades_PotentialSolarHeat_GWh :: Float
         , factsheet   :: String
         , methodology   :: String
           } deriving (Show, Data, Generic)

instance ToJSON Info
instance FromJSON Info where
  parseJSON (Object v) = Info
                         <$> (v .: "MunicipalityNumber")
                         <*> (v .: "MunicipalityName")
                         <*> (v .:? "Canton")
                         <*> (v .: "Country")
                         <*> (v .: "Scenario1_RoofsOnly_PotentialSolarElectricity_GWh")
                         <*> (v .: "Scenario2_RoofsOnly_PotentialSolarElectricity_GWh")
                         <*> (v .: "Scenario2_RoofsOnly_PotentialSolarHeat_GWh")
                         <*> (v .: "Scenario3_RoofsFacades_PotentialSolarElectricity_GWh")
                         <*> (v .: "Scenario4_RoofsFacades_PotentialSolarElectricity_GWh")
                         <*> (v .: "Scenario4_RoofsFacades_PotentialSolarHeat_GWh")
                         <*> (v .: "Factsheet")
                         <*> (v .: "Methodology")

getScenario3:: Info -> Float 
getScenario3 (Info _ _ _ _ _ _ _ x _ _ _ _) = x

getCanton:: Maybe String -> String
getCanton mayStr = case mayStr of
     Just a -> a
     Nothing -> "Not defined"

getStrName:: Info -> String 
getStrName (Info _ name canton country _ _ _ _ _ _ _ _) = "Name: " ++ name ++ " Canton: " ++ getCanton canton ++ " Country: " ++ country 

sumValues:: [Info] -> Float
sumValues [] = 0
sumValues (x:xs) = getScenario3 x + sumValues xs

findThirdLargest:: [Info] -> Info -> Info -> Info -> Info
findThirdLargest [] info1 info2 info3 = info3
findThirdLargest (x:xs) info1 info2 info3 
    | getScenario3 x >= getScenario3 info1 = findThirdLargest xs x info1 info2
    | getScenario3 x >= getScenario3 info2 = findThirdLargest xs info1 x info2
    | getScenario3 x >= getScenario3 info3 = findThirdLargest xs info1 info2 x
    | otherwise = findThirdLargest xs info1 info2 info3


findThirdLargestStart:: [Info] -> Info
findThirdLargestStart [] = error "Error"
findThirdLargestStart (x:[]) = error "Error"
findThirdLargestStart (x:y:[]) = error "Error"
findThirdLargestStart (x:y:z:[]) 
    | getScenario3 x < getScenario3 y && getScenario3 x < getScenario3 z = x
    | getScenario3 y < getScenario3 x && getScenario3 y < getScenario3 z = y
    | otherwise = z
findThirdLargestStart (x:y:z:rest) 
    | getScenario3 x < getScenario3 y && getScenario3 y < getScenario3 z = findThirdLargest rest z y x
    | getScenario3 y < getScenario3 x && getScenario3 x < getScenario3 z = findThirdLargest rest z x y
    | getScenario3 z < getScenario3 x && getScenario3 x < getScenario3 y = findThirdLargest rest y x z
    | getScenario3 x < getScenario3 z && getScenario3 z < getScenario3 y = findThirdLargest rest y z x
    | getScenario3 z < getScenario3 y && getScenario3 y < getScenario3 x = findThirdLargest rest x y z
    | getScenario3 y < getScenario3 z && getScenario3 z < getScenario3 x = findThirdLargest rest x z y


main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Info])
  case d of
    Left err -> Prelude.putStrLn err
    Right ps -> do
      Prelude.putStrLn "Sum of Scenario 3:"
      print $ sumValues ps

      let third = findThirdLargestStart ps 
      Prelude.putStrLn "Information about third largest in Scenario 3:"
      print $ getStrName third
