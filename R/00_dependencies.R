# Setup script with all packages

# Clear workspace
rm(list = ls())

# Install packages only if not previously installed
list_of_packages <- c("httr", "tidyverse", "readxl",
                      "knitr", "plotly", "maps", "patchwork")

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}
