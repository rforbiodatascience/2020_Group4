# Clear workspace
rm(list = ls())

library(tidyverse)
library(httr)
library(readxl)

# Load main data ----------------------------------------------------------
# Load data from url and write as temporary .xlsx file
url <- "http://tropicalpharmacology.com/wp-content/uploads/2020/05/Venomics_Display_data.xlsx"
httr::GET(url = url, write_disk(tf <- tempfile(fileext = ".xlsx")))

# Read .xlsx file and write as csv
df_relative <- read_excel(path = tf, sheet = 2L)
df_relative %>% 
  write_csv('data/_raw/01_data_load_relative.csv')


# Load additional data ----------------------------------------------------
data_new <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=0&single=true&output=csv",
                     col_types = "cnnn")
meta_new <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=1686090584&single=true&output=csv")

# Write data
data_new %>% 
  write_csv('data/_raw/01_data_new.csv')
meta_new %>% 
  write_csv('data/_raw/01_meta_new.csv')
