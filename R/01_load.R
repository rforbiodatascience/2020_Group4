#install.packages(c("httr", "tidyverse", "readxl"))
library(tidyverse)
library(httr)
library(readxl)

url = "http://tropicalpharmacology.com/wp-content/uploads/2018/05/Venomics_Display_data.xlsx"

httr::GET(url = url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tf

df_relative <- read_excel(path = tf, sheet = 1L)
df_absolute <- read_excel(path = tf, sheet = 2L)

#relative_abundance <- read.xlsx(url(), sheetName = "Relative Abundance (%)")
#absolute_abundance <- read.xlsx(, sheetName = "Absolut Abundance (mg)")

