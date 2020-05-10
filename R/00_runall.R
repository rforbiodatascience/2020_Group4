source('R/00_dependencies.R')
source('R/01.1_load_main.R')
source('R/01.2_load_new_data.R')
source('R/02_clean.R')
source('R/03_augment.R')
source('R/04_analysis.R')
#Due to memory limitations in Rstudio Cloud, this code is not able to run here.
#The results of the analysis are therefore obtained by running the exact same code in a new project
#source('R/05_model_ANN.R')
source('R/06_model_PCA.R')
# Shiny app uploaded to: https://shdam.shinyapps.io/compareTwo/