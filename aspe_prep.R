library(hubeau)

# Get the query parameters for the requested API/endpoint
list_params(api = "poisson",
            endpoint = "observations")

# Retrieve selected fields on a river fish sampled in Brest
library(dplyr)
fields <- paste("code_operation",
                "date_operation",
                "code_commune",
                "libelle_point_prelevement_aspe",
                "effectif_lot",
                "code_alternatif_taxon",
                "coordonnee_x_point_prelevement",
                "coordonnee_y_point_prelevement",
                sep = ",")

dpt = c("44", "49", "53", "72", "85")

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
