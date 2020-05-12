library(tidyverse)

# Load data 
new_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=0&single=true&output=csv",
                     col_types = "cnnn")
new_meta <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=1686090584&single=true&output=csv")

# Write data
new_data %>% 
  write_csv('data/_raw/01_new_data.csv')
new_meta %>% 
  write_csv('data/_raw/01_new_meta.csv')
