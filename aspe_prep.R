# Load libraries

library(hubeau)
library(dplyr)
library(readr)
library(tidyr)

# Load species codes

code_espece <- read_csv('./ref_espece.csv')

# Get the query parameters for the requested API/endpoint
list_params(api = "poisson",
            endpoint = "observations")

# Retrieve selected fields on a river fish sampled in Brest

fields <- paste("code_operation",
                "date_operation",
                "code_commune",
                "libelle_station",
                "code_departement",
                "code_station",
                "code_lot",
                "effectif_lot",
                "code_alternatif_taxon",
                "nom_latin_taxon",
                "code_epsg_projection_point_prelevement",
                "coordonnee_x_point_prelevement",
                "coordonnee_y_point_prelevement",
                "protocole_peche",
                sep = ",")

dpt = c("44", "49", "53", "72", "85")
code_sp <-  c("ABH", "ABL", "AMA", "AMB", "AMO", "AN?", "ANC", "ANG", "APC", "APE", "APH",
                             "APP", "APR", "APT", "ASA", "ASL", "ASP", "AT?", "ATB", "ATH",
                             "ATS", "ATY", "AWA", "BAE", "BAF", "BAM", "BAX", "BBB", "BBG",
                             "BBP", "BBX", "BLX", "BLE", "BLN", "BOU", "BRB", "BRD", "BRE",
                             "BRO", "BRX", "CAD", "CAA", "CAG", "CAK", "CAR", "CAS", "CAT",
                             "CAX", "CCA", "CCO", "CCU", "CCX", "CDR", "CGR", "CGT", "CHA",
                             "CHC", "CHE", "CHP", "CLU", "CMI", "COA", "COR", "CPV", "CRB",
                             "CRC", "CRE", "CRG", "CRI", "CTI", "CYP", "ECR", "ELF", "ELM",
                             "EPE", "EPI", "EPT", "ES?", "EST", "FLE", "GAM", "GAR", "GBN",
                             "GDL", "GFL", "GKS", "GLO", "GOA", "GOB", "GOK", "GOL", "GON",
                             "GOO", "GOU", "GOX", "GRC", "GRE", "GRI", "GRT", "GRV", "GTN",
                             "GUP", "HAR", "HOT", "HOX", "HUC", "HBG", "IDE", "KUL", "LAN",
                             "LIJ", "LIP", "LOB", "LOE", "LOF", "LOM", "LOR", "LOT", "LOU",
                             "LOX", "LPM", "LPP", "LPR", "LPX", "MAA", "MAC", "MAH", "MAI",
                             "MAL", "MAT", "MER", "MGL", "MIC", "MOT", "MUC", "MUD", "MUP",
                             "MUS", "OBL", "OBR", "OCL", "OME", "OMI", "ONI", "ORE", "PAP",
                             "PCC", "PCH", "PER", "PES", "PFL", "PHX", "PIM", "PLI", "POB",
                             "PRX", "PSR", "PTM", "RBC", "ROT", "ROX", "RUB", "SAL", "SAN",
                             "SAR", "SAT", "SCH", "SCO", "SDF", "SIC", "SIL", "SOL", "SPI",
                             "SPT", "SQX", "STE", "STL", "SYN", "TAC", "TAD", "TAN", "TFA",
                             "TIL", "TOX", "TRC", "TRF", "TRL", "TRM", "UMP", "VAB", "VAC",
                             "VAI", "VAL", "VAN", "VAR", "VCU", "VIM", "VRO", "XIP", "YIR",
                             "OCI", "OCJ", "PCF", "OCV", "LP?", "OPG", "OPX", "GBT", "PCV",
                             "ESX", "SRO", "FAR", "ROI", "GAX", "BLI", "OBX", "BRA", "BRI",
                             "GAH", "OBA", "VAD", "VAF")



fish_pdl <- NULL # initiate empy dataframe to store data
tmp <- NULL # empty temporary object for the loop

for (i in dpt) { #looping through the list of departements
  for (j in code_sp){ #looping through the fish species list
    tmp <- get_poisson_observations( # use hubeau query to get the data for each
    #species in each departement
      list(code_departement = i,
           code_alternatif_taxon = j,
           fields = fields))
    if (nrow(tmp) == 0){ # when a species is not present in a departement, an empty tibble
      # is returned which will cause an error in the group by... 
      next # So we skip it !
    }
    tmp <- tmp %>% 
      group_by_at(vars(code_operation, date_operation, code_departement, code_commune, code_station,
                     libelle_station, code_lot, code_alternatif_taxon, nom_latin_taxon,
                     code_epsg_projection_point_prelevement, coordonnee_x_point_prelevement, 
                     coordonnee_y_point_prelevement, protocole_peche)) %>%
      summarise(denbrMin = sum(effectif_lot))
    fish_pdl = bind_rows(fish_pdl, tmp)
  }}

aspe2import <- fish_pdl %>% 
  filter(code_epsg_projection_point_prelevement == '2154' & denbrMin > 0) %>% 
  left_join(code_espece, by = c("code_alternatif_taxon" = "esp_code_alternatif")) %>% 
  mutate(statObs = "Pr",
         dateFin = date_operation,
         denbrMax = denbrMin,
         objDenbr = "IND",
         ocStatBio = 2,
         ocNat = 1,
         ocEtatBio = 2,
         statSource = "Te",
         dispColl = "Observation",
         obsId = "Anonyme",
         detId = "Anonyme",
         obsNomOrg = "OFB",
         detNomOrg = "OFB",
         orgGestDat = "OFB",
         vTAXREF = "v.14.0",) %>% 
  rename(idOrigine = code_lot,
         cdDep = code_departement,
         cdCommune = code_commune,
         nomLieu = libelle_station,
         xL93 = coordonnee_x_point_prelevement,
         yL93 = coordonnee_y_point_prelevement,
         cdNom = esp_code_taxref,
         nomCite = nom_latin_taxon,
         dateDebut = date_operation)
         
write_csv(aspe2import, file = "./221108_aspe2import.csv")



