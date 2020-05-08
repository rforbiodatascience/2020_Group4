library(tidyverse)
library(httr)
library(readxl)

# Load data from url and write as temporary .xlsx file
url = "http://tropicalpharmacology.com/wp-content/uploads/2020/05/Venomics_Display_data.xlsx"
httr::GET(url = url, write_disk(tf <- tempfile(fileext = ".xlsx")))

# Read .xlsx file and write as csv
df_relative <- read_excel(path = tf, sheet = 2L)
df_relative %>% 
  write_csv('data/_raw/01_data_load_relative.csv')