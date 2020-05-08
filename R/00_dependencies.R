# Setup script with all packages
packages <- c(require("httr"), require("tidyverse"), require("readxl"), 
              require("googlesheets4"), require("knitr"), require("plotly"))
install.packages(packages)
