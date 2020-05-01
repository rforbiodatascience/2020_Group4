library(tidyverse)
library(dplyr)
library(stringr)

## Load raw data
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")

## Clean toxin names (Kan overvejes at undlades, da det er rart at kende enheden)
#rm_percent <- function(string){
#  string <- string %>% 
#    str_sub(start = 1, end = str_length(string)-3)
#  return(string)
#}
#relative <- relative %>% 
#  rename_if(is_double, rm_percent)

## Clean note column and rename to country
# Søg efter stjerner i snake column og fjern dem (transcriptomics)
# Find unikke navne for note og gruppér (stater --> land)

clean_col <- function(data, str_){
  data <- data %>% 
    filter_all(all_vars(!grepl(str_,.)))
  return (data)
}

data_clean <- clean_col(data_raw,"unknown")
#Condition in data set is that rownames ending with * indicates transcriptomic data
data_clean <- clean_col(data_clean,"\\*")

#Change name of column to country
colnames(data_clean)[3] <- "Country" 

clean_country <- function(){
  
  
}


## Remove toxins with low sums
summed_toxins <- relative %>% 
  select(-c("Snake", "Reference", "Note", "Sum")) %>% 
  # colSums() 
  summarise_all(list(sum)) %>% 
  pivot_longer(everything()) %>%
  filter(value > 10)

relative <- relative %>% 
  select(c("Snake", "Reference", "Note", summed_toxins$name))


# Write cleaned data
relative %>% 
  write_csv('data/02_relative_clean.csv')

## Add new data
relative <- relative %>% 
  rename(`SP (Serine Proteinase)` = "SP (Serine roteinase)",
         `α-NTx (α-NeuroToxin)` = "?-NTx (?-NeuroToxin)")

new_data <- read_csv("data/01.2_new_data.csv")

relative_new <- relative %>% 
  full_join(new_data) %>%
  mutate_each(list(~replace(., which(is.na(.)), 0)))

relative_new %>% 
  write_csv('data/02_relative_new_clean.csv')
