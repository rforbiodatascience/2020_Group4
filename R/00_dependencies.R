# Setup script with all packages
list_of_packages <- c("httr", "tidyverse", "readxl", "shiny", "devtools",
                      "knitr", "plotly", "maps", "patchwork", "rsconnect", "keras")

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}