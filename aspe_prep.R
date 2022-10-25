# Load libraries

library(hubeau)
library(dplyr)
library(readr)
library(tidyr)

# Load Sandre referenciel

download.file('https://api.sandre.eaufrance.fr/referentiels/v1/apt.csv?compress=FALSE',
              destfile = 'ref_sandre.csv')

ref_sandre <- read_csv2('./ref_sandre.csv')
ref_fish_sandre <- ref_sandre[-1,] %>% 
  filter(StAppelTaxon == 'Validé' & LbThemeTaxon == 'Poissons' &
           LbNiveauTaxonomique == 'Espèce')

# Get the query parameters for the requested API/endpoint
list_params(api = "poisson",
            endpoint = "observations")

# Retrieve selected fields on a river fish sampled in Brest

fields <- paste("code_operation",
                "date_operation",
                "code_commune",
                "libelle_point_prelevement_aspe",
                "effectif_lot",
                "code_alternatif_taxon",
                "coordonnee_x_point_prelevement",
                "coordonnee_y_point_prelevement",
                "protocole_peche",
                sep = ",")

dpt = c("44", "49", "53", "72", "85")
code_sp <-  c("ABH", "ABI", "ABL", "AGO", "ALA", "ALF", "ALR", "ALX",
              "AMA", "AMB", "AMO", "AN?", "ANC", "ANG", "APC", "APE", "APH",
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


 dpt = c("44", "49")
 code_sp <-  c("ANG", "ABL")

ang_pdl <- for (i in dpt) {
  get_poisson_observations(
    list(
      code_departement = i,
      code_alternatif_taxon = "ANG",
      fields = fields
  )
) %>%
    group_by_at(vars(-effectif_lot)) %>%
    summarise(nb_individals = sum(effectif_lot))
  }



brest_fishes <- get_poisson_observations(
  list(
    libelle_region = "PAYS-DE-LA-LOIRE",
    #libelle_commune = "Nantes",
    code_departement = "44",
    code_alternatif_taxon = "ANG",
    fields = fields
  )
) %>%
  group_by_at(vars(-effectif_lot)) %>%
  summarise(nb_individals = sum(effectif_lot))


fish_pdl <- NULL

for (i in dpt) {
  for (j in code_sp){
  tmp <- get_poisson_observations(
    list(code_departement = i,
         code_alternatif_taxon = j,
         fields = fields))
  fish_pdl = bind_rows(fish_pdl, tmp) %>%
    #group_by_at(vars(-effectif_lot)) %>%
    group_by(-effectif_lot)
    summarise(nb_individals = sum(effectif_lot))
}}

  
