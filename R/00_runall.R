rm(list = ls())

source("R/00_dependencies.R")
source("R/01_load_data.R")
source("R/02_clean.R")
source("R/03_augment.R")
source("R/04_snake_distribution.R")
source("R/05_venom_composition.R")
source("R/06_unsupervised_analysis.R")
# Due to storage limitations in Rstudio Cloud, this code is not able to run here.
# The results of the analysis are therefore obtained by running the exact same code in a new project
# source("R/07_ANN_family.R")
# source("R/07_ANN_continents.R")
# Shiny app uploaded to: https://shdam.shinyapps.io/compareTwo/
# Shiny code: "R/08_shinyapp.R"