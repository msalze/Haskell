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

packStr :: String -> ByteString
packStr input = C.pack input

data Info =
  Info { municipalityNumber  :: Int
         , municipalityName   :: String
         , canton   :: String
         , country   :: String
         , scenario1_RoofsOnly_PotentialSolarElectricity_GWh :: Int
         , scenario2_RoofsOnly_PotentialSolarElectricity_GWh :: Int
         , scenario2_RoofsOnly_PotentialSolarHeat_GWh :: Int
         , scenario3_RoofsFacades_PotentialSolarElectricity_GWh :: Int
         , scenario4_RoofsFacades_PotentialSolarElectricity_GWh :: Int
         , scenario4_RoofsFacades_PotentialSolarHeat_GWh :: Int
         , factsheet   :: String
         , methodology   :: String
           } deriving (Show, Data, Generic)

instance FromJSON Info
-- instance FromJSON Info where
--   parseJSON (Object v) = Info
--                          <$> (v .: "municipalityNumber")
--                          <*> (v .: "municipalityName")
--                          <*> (v .: "canton")
--                          <*> (v .: "country")
--                          <*> (v .: "scenario1_RoofsOnly_PotentialSolarElectricity_GWh")
--                          <*> (v .: "scenario2_RoofsOnly_PotentialSolarElectricity_GWh")
--                          <*> (v .: "scenario2_RoofsOnly_PotentialSolarHeat_GWh")
--                          <*> (v .: "scenario3_RoofsFacades_PotentialSolarElectricity_GWh")
--                          <*> (v .: "scenario4_RoofsFacades_PotentialSolarElectricity_GWh")
--                          <*> (v .: "scenario4_RoofsFacades_PotentialSolarHeat_GWh")
--                          <*> (v .: "factsheet")
--                          <*> (v .: "methodology")

getScenario3:: Info -> Int 
getScenario3 (Info _ _ _ _ _ _ _ x _ _ _ _) = x

getStrName:: Info -> String 
getStrName (Info _ name canton country _ _ _ _ _ _ _ _) = "Name: " ++ name ++ " Canton: " ++ canton ++ " Country: " ++ country 

sumValues:: [Info] -> Int
sumValues [] = 0
sumValues (x:xs) = getScenario3 x + sumValues xs

findThirdLargest:: [Info] -> Info -> Info -> Info -> Info
findThirdLargest [] info1 info2 info3 = info3
findThirdLargest (x:xs) info1 info2 info3 
    | getScenario3 x > getScenario3 info1 = findThirdLargest xs x info1 info2
    | getScenario3 x > getScenario3 info2 = findThirdLargest xs info1 x info2
    | getScenario3 x > getScenario3 info3 = findThirdLargest xs info1 info2 x

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
    -- let json = "{\"MunicipalityNumber\" : 1, \"MunicipalityName\" : \"Aeugst am Albis\", \"Canton\" : \"ZÃ¼rich\", \"Country\" : \"CH\", \"Scenario1_RoofsOnly_PotentialSolarElectricity_GWh\" : 13.43, \"Scenario2_RoofsOnly_PotentialSolarElectricity_GWh\" : 8.7, \"Scenario2_RoofsOnly_PotentialSolarHeat_GWh\" : 4.72, \"Scenario3_RoofsFacades_PotentialSolarElectricity_GWh\" : 18.06, \"Scenario4_RoofsFacades_PotentialSolarElectricity_GWh\" : 13.33, \"Scenario4_RoofsFacades_PotentialSolarHeat_GWh\" : 4.72, \"Factsheet\" : \"https://www.uvek-gis.admin.ch/BFE/storymaps/ECH_SolarpotGemeinden/pdf/1.pdf\", \"Methodology\" : \"https://www.uvek-gis.admin.ch/BFE/redirect/sol.html\"}"
    -- let test = (decodeJSON json :: Info)

    -- print $ typeOf test

    response <- simpleHTTP $ getRequest "http://www.uvek-gis.admin.ch/BFE/ogd/52/Solarenergiepotenziale_Gemeinden_Daecher_und_Fassaden.json"
    let body = fmap rspBody response -- :: Either ConnError ByteString
    -- print response
    -- print body

    print $ typeOf response
    print $ typeOf body
    -- -- let var = Data.Aeson.decode body

    case body of
      Left err -> print "Error"
      Right msg -> do
    --     let byteStr = packStr msg
    --     print $ typeOf byteStr

      let mm = parseJSON msg :: Info

      print $ typeOf mm


        
    --     let vaar = Data.Aeson.decode byteStr :: [Info] 
    --     print $ typeOf vaar
    --     -- let vaar = Prelude.concat . maybeToList $ Data.Aeson.decode $ packStr msg
    --     -- Prelude.putStrLn $ show vaar


    -- case isLeft body of
    --   True -> print "Error"
    --   False -> do 
    --     let vaar = Right body
    --     print $ show vaar
      -- False -> print $ show Prelude.concat . maybeToList $ Data.Aeson.decode $ body

    -- let test2 = 

    -- print $ show 


    -- print $ show var

    -- input <- B.readFile "http://www.uvek-gis.admin.ch/BFE/ogd/52/Solarenergiepotenziale_Gemeinden_Daecher_und_Fassaden.json"

    -- let byteStr = packStr msg
    -- print $ typeOf byteStr
    -- let mm = Data.Aeson.decode response
  

    -- print $ typeOf mm
    -- case mm of
    --   Nothing -> print "error parsing JSON"
    --   Just m -> putStrLn $ show m
    -- example value 
    -- let values = [] :: [Info]

    -- let info1 = Info (1) ("1") ("1") ("1") (1) (1) (1) (1) (1) (1) ("") ("") 
    -- let info2 = Info (2) ("2") ("2") ("2") (2) (2) (2) (20) (2) (2) ("") ("") 
    -- let info3 = Info (3) ("3") ("3") ("3") (3) (3) (3) (30) (3) (3) ("") ("") 
    -- let info4 = Info (4) ("4") ("4") ("4") (4) (4) (4) (40) (4) (4) ("") ("") 

    -- let infoList = [info3, info1, info4, info2]

    -- print $ sumValues infoList

    -- let third = findThirdLargestStart infoList 
    -- print $ getStrName third


    -- let infoList2 = [test, test, test, test]

    -- print $ sumValues infoList2

    -- let third2 = findThirdLargestStart infoList2 
    -- print $ getStrName third2


