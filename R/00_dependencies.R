# Setup script with all packages
list.of.packages <- c("httr", "tidyverse", "readxl", "googlesheets4", 
                      "shiny", "knitr", "plotly", "maps", "patchwork")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if( length(new.packages) ) {
  install.packages(new.packages)
}



