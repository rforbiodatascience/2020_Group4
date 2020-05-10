library(googlesheets4)
library(tidyverse)
library(httr)
library(readxl)

# Load data ---------------------------------------------------------------
new_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=0&single=true&output=csv",
                     col_types = "cnnn")
new_meta <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=1686090584&single=true&output=csv")

# Tidy data ---------------------------------------------------------------
new_data <- new_data %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(-Toxin, names_to = "Snake", values_to = "value") %>%
  pivot_wider(names_from = Toxin, values_from = value) %>%
  left_join(new_meta, by = "Snake") %>% 
  mutate(`Unknown/Undetermined` = 100 - Reduce(`+`, select_if(., is.numeric)))

# Write data --------------------------------------------------------------
new_data %>% 
  write_csv('data/_raw/01.2_new_data.csv')
