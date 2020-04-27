library(tidyverse)

# Load raw data
relative <- read_csv("data/_raw/relative.csv")

# Clean data

## Clean toxin names
rm_percent <- function(string){
  string <- string %>% 
    str_sub(start = 1, end = str_length(string)-3)
  return(string)
}

relative <- relative %>% 
  rename_if(is_double, rm_percent)

## Clean note column and rename to country


## Remove toxins with low sums
summed_toxins <- relative %>% 
  select(-c("Snake", "Reference", "Note", "Sum, %")) %>% 
  # colSums() 
  summarise_all(list(sum)) %>% 
  pivot_longer(everything()) %>%
  filter(value > 10)

relative <- relative %>% 
  select(c("Snake", "Reference", "Note", summed_toxins$name))

# Write cleaned data
relative %>% 
  write_csv('data/relative_clean.csv')