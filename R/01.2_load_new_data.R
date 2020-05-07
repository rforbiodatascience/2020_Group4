# Add new data
# install.packages("googlesheets4")

library(googlesheets4)
library(tidyverse)


# Load data ---------------------------------------------------------------
new_data <- read_sheet('https://docs.google.com/spreadsheets/d/1vLrvvQmQdvCtr6n0hjDbIoLiOv3cHVGx7MK_2w_WH3E/edit#gid=0',
                       sheet = 1,
                       col_types = 'cnnn')
new_meta <- read_sheet('https://docs.google.com/spreadsheets/d/1vLrvvQmQdvCtr6n0hjDbIoLiOv3cHVGx7MK_2w_WH3E/edit#gid=0',
                       sheet = 2)
# Tidy data ---------------------------------------------------------------
new_data <- new_data %>% 
  replace(is.na(.), 0) %>% 
  pivot_longer(-Toxin, names_to = "Snake", values_to = "value") %>%
  pivot_wider(names_from = Toxin, values_from = value) %>%
  left_join(new_meta, by = "Snake") %>% 
  mutate(`Unknown/Undetermined` = 100 - Reduce(`+`, select_if(., is.numeric))) %>% 
  rename(Region = Country)



# Write data --------------------------------------------------------------
new_data %>% 
  write_csv('data/01.2_new_data.csv')
