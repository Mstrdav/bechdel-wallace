# Liste des packages nécessaires pour le projet
packages_requis <- c("shiny", "DT", "plotly", "dplyr", "data.table", 
                     "arrow", "cachem", "bslib", "bsicons", "thematic", "shinyWidgets")

# Installation des packages manquants uniquement
packages_a_installer <- packages_requis[!(packages_requis %in% installed.packages()[,"Package"])]
if(length(packages_a_installer)) {
  install.packages(packages_a_installer)
} else {
  message("Tous les packages sont déjà installés !")
}