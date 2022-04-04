import Data.Text
import Json.Decode
-- http://www.uvek-gis.admin.ch/BFE/ogd/52/Solarenergiepotenziale_Gemeinden_Daecher_und_Fassaden.json

data Info =
  Info { municipalityNumber  :: Int
         , municipalityName   :: !Text
         , canton   :: !Text
         , country   :: !Text
         , scenario1_RoofsOnly_PotentialSolarElectricity_GWh :: Int
         , scenario2_RoofsOnly_PotentialSolarElectricity_GWh :: Int
         , scenario2_RoofsOnly_PotentialSolarHeat_GWh :: Int
         , scenario3_RoofsFacades_PotentialSolarElectricity_GWh :: Int
         , scenario4_RoofsFacades_PotentialSolarElectricity_GWh :: Int
         , scenario4_RoofsFacades_PotentialSolarHeat_GWh :: Int
         , factsheet   :: !Text
         , methodology   :: !Text
           } deriving Show

main = do
    let json = "{\"MunicipalityNumber\" : 1, \"MunicipalityName\" : \"Aeugst am Albis\", \"Canton\" : \"ZÃ¼rich\", \"Country\" : \"CH\", \"Scenario1_RoofsOnly_PotentialSolarElectricity_GWh\" : 13.43, \"Scenario2_RoofsOnly_PotentialSolarElectricity_GWh\" : 8.7, \"Scenario2_RoofsOnly_PotentialSolarHeat_GWh\" : 4.72, \"Scenario3_RoofsFacades_PotentialSolarElectricity_GWh\" : 18.06, \"Scenario4_RoofsFacades_PotentialSolarElectricity_GWh\" : 13.33, \"Scenario4_RoofsFacades_PotentialSolarHeat_GWh\" : 4.72, \"Factsheet\" : \"https://www.uvek-gis.admin.ch/BFE/storymaps/ECH_SolarpotGemeinden/pdf/1.pdf\", \"Methodology\" : \"https://www.uvek-gis.admin.ch/BFE/redirect/sol.html\"}"
    print (decodeJSON json :: Info)
