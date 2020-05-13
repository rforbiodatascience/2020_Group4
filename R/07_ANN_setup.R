# Setup session
# Artificial Neural Networks using Keras / Tensorflow in R

rm(list = ls())

# Load libraries ----------------------------------------------------------
# Install packages if not previously installed
list_of_packages <- c("tidyverse", "devtools")

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}

# Install keras -----------------------------------------------------------

install_tensorflow <- function(){
  library("devtools")
  install_github("rstudio/keras", force = TRUE)
  # Would you like to install miniconda? Y
  library(keras)
  install_keras(tensorflow = "1.13.1")
}

if("tensorflow" %in% installed.packages()[, "Package"]){
  if(installed.packages()["tensorflow", "Version"] != "1.13.1"){
    install_tensorflow()
  }
}else{
  install_tensorflow()
}
