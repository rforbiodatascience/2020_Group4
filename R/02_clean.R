library(tidyverse)
library(dplyr)
library(stringr)

###### Load raw data
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")

###### Clean note column and rename to country
clean_col <- function(data, str_){
  data <- data %>% 
    filter_all(all_vars(!grepl(str_,.)))
  return (data)
}

#Condition in data set is that rownames ending with * indicates transcriptomic data
data_clean <- clean_col(data_raw,"unknown")
data_clean <- clean_col(data_clean,"\\*")     


data_test <- data_clean
unik <- unique(data_clean[3])

data_test <- data_test %>% 
  mutate(Region = case_when(Note == "Texas" ~ "USA", 
                            Note == "Kentucky" ~ "USA",
                            Note == "Missouri" ~ "USA",
                            Note == "Florida" ~ "USA",
                            Note == "Caribbean neonate" ~ "Caribbean",
                            Note == "Caribbean adult" ~ "Caribbean",
                            Note == "Pacific adult" ~ "Pacific",
                            Note == "Pacific neonate" ~ "Pacific",
                            Note == "Costa Rica (Pacific)" ~ "Costa Rica",
                            Note == "Costa Rica (Carribean)" ~ "Costa Rica",
                            Note == "Columbia" ~ "Colombia",
                            Note == "Venezuelan juvenile" ~ "Venezuela",
                            Note == "Venezuelan adult" ~ "Venezuela",
                            Note == "Carribean" ~ "Caribbean",
                            Note == "Juazeiro" ~ "Brazil",
                            Note == "Ceara" ~ "Brazil",
                            Note == "Paraiba" ~ "Brazil",
                            Note == "Pernambuco" ~ "Brazil",
                            Note == "ilha de Itaparica" ~ "Brazil",
                            Note == "Adult and young in Brazil" ~ "Brazil",
                            Note == "Catalina Island 1" ~ "USA",
                            Note == "Phelan 1" ~ "USA",
                            Note == "Idyllwild 1" ~ "USA",
                            Note == "Phelan 2" ~ "USA",
                            Note == "Loma Linda 1" ~ "USA",
                            Note == "Idyllwild 3" ~ "USA",
                            Note == "Idyllwild 2" ~ "USA",
                            Note == "Phelan 3" ~ "USA",
                            Note == "Loma Linda 2" ~ "USA",
                            Note == "Catalina Island 2" ~ "USA",
                            Note == "Catalina Island 3" ~ "USA",
                            Note == "Arizona (A-101)" ~ "USA",
                            Note == "Arizona (F-303)" ~ "USA",
                            Note == "Arizona (F-307)" ~ "USA",
                            Note == "Arizona (E-105)" ~ "USA",
                            Note == "Arizona (E-104)" ~ "USA",
                            Note == "Arizona (E-106)" ~ "USA",
                            Note == "Arizona (E-203)" ~ "USA",
                            Note == "New Mexico (F-301)" ~ "USA",
                            Note == "Arizona (A-103)" ~ "USA",
                            Note == "Arizona (E-202)" ~ "USA",
                            Note == "Arizona (E-204)" ~ "USA",
                            Note == "Arizona (A-108)" ~ "USA",
                            Note == "Arizona (B-110)" ~ "USA",
                            Note == "Arizona (C-107)" ~ "USA",
                            Note == "Arizona (D-201)" ~ "USA",
                            Note == "New Mexico (F-302)" ~ "USA",
                            Note == "New Mexico (F-304)" ~ "USA",
                            Note == "Arizona (F-306)" ~ "USA",
                            Note == "Arizona (D-109)" ~ "USA",
                            Note == "New Mexico (F-305)" ~ "USA",
                            Note == "Arizona (A-102)" ~ "USA",
                            Note == "Costa Rican adult" ~ "Costa Rica",
                            Note == "Costa Rican neonate (6-week-old)" ~ "Costa Rica",
                            Note == "Mexican adult" ~ "Mexico",
                            Note == "Mexican neonate" ~ "Mexico",
                            Note == "Colorado (adult female)" ~ "USA",
                            Note == "Colorado (adult male)" ~ "USA",
                            Note == "Colorado (neonate female)" ~ "USA",
                            Note == "Colorado (neonate male)" ~ "USA",
                            Note == "Woodlark island" ~ "Papua New Guinea",
                            Note == "Kansas" ~ "USA",
                            Note == "Colorado" ~ "USA",
                            Note == "Ohio" ~ "USA",
                            Note == "Forida" ~ "USA",
                            Note == "Saibai Island" ~ "Australia",
                            Note == "Costa Rican Golfo de Papagayo" ~ "Costa Rica",
                            Note == "Java Island" ~ "Indonesia",
                            Note == "East India" ~ "India",
                            Note == "North-west India" ~ "India",
                            Note == "Costa Rican neonate" ~ "Costa Rica",
                            Note == "Chinese adult" ~ "China",
                            Note == "West India" ~ "India"))

data_test %>% 
  coalesce(Region, Note)

#To do: 
# Ryk kolonnen med countries, som en af de f??rste
# Slet Note kolonnen
# 

#data_test <- data_clean
#data_test$Country <- as.character(data_test$Country) %>% 
#  revalue(c("Kentucky"="USA", "Texas"="USA", "Missouri"="USA", "Florida"="USA", "Arizona*"="USA", "New Mexico"="USA"))



"[:punct:]"

# Find unikke navne for note og grupp??r (stater --> land)

#brazil: 
#Juazeiro
#Ceara
#Paraiba
#Pernambuco
#ilha de Itaparica
















###### Clean toxin names (Kan overvejes at undlades, da det er rart at kende enheden)
#rm_percent <- function(string){
#  string <- string %>% 
#    str_sub(start = 1, end = str_length(string)-3)
#  return(string)
#}
#relative <- relative %>% 
#  rename_if(is_double, rm_percent)

is_not_zero <- function(data){
  not_zero <- sum(data != 0)
  return(not_zero)
}

# d %>% summarise(not_zero = sum(my_var != 0)) 
###### Remove toxins with few occurances
summed_toxins <- relative %>% 
  select_if(is.numeric) %>% 
  
  # summarise_if(if_else(. > 0, 1, 0), n) %>%
  # pivot_longer(everything()) %>%
  # mutate(positive = 
  #          case_when(value > 0 ~ TRUE,
  #                    value == 0 ~ FALSE)) %>% 
  # group_by(name) %>% 
  # summarise(sum(positive)) %>% 
  filter(`sum(positive)` > 5) %>% 
  View()

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
