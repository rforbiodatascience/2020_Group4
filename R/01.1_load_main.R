#install.packages(c("httr", "tidyverse", "readxl"))
library(tidyverse)
library(httr)
library(readxl)

url = "http://tropicalpharmacology.com/wp-content/uploads/2020/05/Venomics_Display_data.xlsx"

httr::GET(url = url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tf

df_relative <- read_excel(path = tf, sheet = 1L)
df_absolute <- read_excel(path = tf, sheet = 2L)

df_relative %>% 
  write_csv('data/_raw/01_data_load_relative.csv')
df_absolute %>% 
  write_csv('data/_raw/01_data_load_absolute.csv')

#relative_abundance <- read.xlsx(url(), sheetName = "Relative Abundance (%)")
#absolute_abundance <- read.xlsx(, sheetName = "Absolut Abundance (mg)")
