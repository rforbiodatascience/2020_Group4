# ------------------------------------------------------------------------------
# Setup session:
# Artificial Neural Networks using Keras / Tensorflow in R
# ------------------------------------------------------------------------------
rm(list = ls())

# Install packages if not previously installed
# ------------------------------------------------------------------------------
list_of_packages <- c("tidyverse", "devtools")

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("devtools")

# Install keras
# ------------------------------------------------------------------------------
install_github("rstudio/keras", force = TRUE)
# Would you like to install miniconda? Y
library(keras)
install_keras(tensorflow = "1.13.1")