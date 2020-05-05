library(tidyverse)
library(dplyr)
library(stringr)

###### Load raw data
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")

# Virker ikke endnu..
detect_in_list <- function(string, list){
  string <- str_split_fixed(string, " ", 2)[1]
  if(string %in% list){
    return(TRUE)
  }else(
    return(FALSE)
  )
}
###### Clean note column and add new column containing grouped regions
#Condition in data set is that rownames ending with * indicates transcriptomic data
Brazilian_cities <- c("Juazeiro", "Ceara", "Paraiba", "Pernambuco", "ilha de Itaparica", "Adult and young in Brazil")
USA <- c("Colorado", "Arizona", "Idyllwild", "Phelan", "Catalina", "Texas", "Kentucky", "Missouri",
         "Florida", "Kansas", "Colorado", "Ohio")
data_clean <- data_raw %>% 
  filter(!(str_to_lower(Note) %in% c("origin unknown", "pooled", "neonate", "adult")),
         str_detect(Snake, '\\*', negate = TRUE)) %>% 
  # filter_all(all_vars(!grepl("\\*",.))) %>% 
  mutate(new_col = case_when(
                            Note %in% USA ~ "USA",
                            # detect_in_list(Note, USA) ~ "USA",
                            str_detect(Note, "New Mexico") ~ "USA",
                            # Note == "Texas" ~ "USA", 
                            # Note == "Kentucky" ~ "USA",
                            # Note == "Missouri" ~ "USA",
                            # Note == "Florida" ~ "USA",
                            str_detect(Note, "Caribbean") ~ "Caribbean",
                            # Note == "Caribbean neonate" ~ "Caribbean",
                            # Note == "Caribbean adult" ~ "Caribbean",
                            str_detect(Note, "Pacific") ~ "Pacific",
                            # Note == "Pacific adult" ~ "Pacific",
                            # Note == "Pacific neonate" ~ "Pacific",
                            str_detect(str_to_lower(Note), "costa rica") ~ "Costa Rica",
                            # Note == "Costa Rica (Pacific)" ~ "Costa Rica",
                            # Note == "Costa Rica (Carribean)" ~ "Costa Rica",
                            Note == "Columbia" ~ "Colombia",
                            str_detect(Note, "Venezuelan") ~ "Venezuela",
                            # Note == "Venezuelan juvenile" ~ "Venezuela",
                            # Note == "Venezuelan adult" ~ "Venezuela",
                            Note %in% Brazilian_cities ~ "Brazil",
                            Note == "Carribean" ~ "Caribbean",
                            Note == "Marocco" ~ "Morocco",
                            Note == "Tunesia" ~ "Tunisia",
                            # Note == "Juazeiro" ~ "Brazil",
                            # Note == "Ceara" ~ "Brazil",
                            # Note == "Paraiba" ~ "Brazil",
                            # Note == "Pernambuco" ~ "Brazil",
                            # Note == "ilha de Itaparica" ~ "Brazil",
                            # Note == "Adult and young in Brazil" ~ "Brazil",
                            # Note == "Catalina Island 1" ~ "USA",
                            # Note == "Phelan 1" ~ "USA",
                            # Note == "Idyllwild 1" ~ "USA",
                            # Note == "Phelan 2" ~ "USA",
                            Note == "Loma Linda 1" ~ "USA",
                            # Note == "Idyllwild 3" ~ "USA",
                            # Note == "Idyllwild 2" ~ "USA",
                            # Note == "Phelan 3" ~ "USA",
                            Note == "Loma Linda 2" ~ "USA",
                            # Note == "Catalina Island 2" ~ "USA",
                            # Note == "Catalina Island 3" ~ "USA",
                            # Note == "Arizona (A-101)" ~ "USA",
                            # Note == "Arizona (F-303)" ~ "USA",
                            # Note == "Arizona (F-307)" ~ "USA",
                            # Note == "Arizona (E-105)" ~ "USA",
                            # Note == "Arizona (E-104)" ~ "USA",
                            # Note == "Arizona (E-106)" ~ "USA",
                            # Note == "Arizona (E-203)" ~ "USA",
                            Note == "New Mexico (F-301)" ~ "USA",
                            # Note == "Arizona (A-103)" ~ "USA",
                            # Note == "Arizona (E-202)" ~ "USA",
                            # Note == "Arizona (E-204)" ~ "USA",
                            # Note == "Arizona (A-108)" ~ "USA",
                            # Note == "Arizona (B-110)" ~ "USA",
                            # Note == "Arizona (C-107)" ~ "USA",
                            # Note == "Arizona (D-201)" ~ "USA",
                            Note == "New Mexico (F-302)" ~ "USA",
                            Note == "New Mexico (F-304)" ~ "USA",
                            # Note == "Arizona (F-306)" ~ "USA",
                            # Note == "Arizona (D-109)" ~ "USA",
                            Note == "New Mexico (F-305)" ~ "USA",
                            # Note == "Arizona (A-102)" ~ "USA",
                            # Note == "Costa Rican adult" ~ "Costa Rica",
                            # Note == "Costa Rican neonate (6-week-old)" ~ "Costa Rica",
                            # Note == "Costa rica" ~ "Costa Rica",
                            str_detect(Note, "Mexican") ~ "Mexico",
                            # Note == "Mexican adult" ~ "Mexico",
                            # Note == "Mexican neonate" ~ "Mexico",
                            Note == "Colorado (adult female)" ~ "USA",
                            Note == "Colorado (adult male)" ~ "USA",
                            Note == "Colorado (neonate female)" ~ "USA",
                            Note == "Colorado (neonate male)" ~ "USA",
                            Note == "Woodlark island" ~ "Papua New Guinea",
                            # Note == "Kansas" ~ "USA",
                            # Note == "Colorado" ~ "USA",
                            # Note == "Ohio" ~ "USA",
                            # Note == "Forida" ~ "USA",
                            Note == "Saibai Island" ~ "Australia",
                            # Note == "Costa Rican Golfo de Papagayo" ~ "Costa Rica",
                            str_detect(Note, 'India') ~ "India",
                            Note == "Java Island" ~ "Indonesia",
                            # Note == "East India" ~ "India",
                            # Note == "North-west India" ~ "India",
                            # Note == "Costa Rican neonate" ~ "Costa Rica",
                            Note == "Chinese adult" ~ "China")) %>% 
                            # Note == "West India" ~ "India")) %>% 
  mutate(Region = coalesce(new_col, Note)) %>%
  select(c(-Note, -new_col, -`Sum, %`)) %>% 
  count(Region) %>% 
  View()

#%>% 
 # subset(data_clean, select=c(1,"Region",3:-1))
   

#To do: 
# Ryk kolonnen med countries, som en af de f??rste
















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

###### Remove toxins with few occurances
summed_toxins <- data_clean %>% 
  select_if(is.numeric) %>% 
  summarise_all(is_not_zero) %>% 
  pivot_longer(everything(), values_to = 'toxin_occurance', names_to = 'toxin') %>%
  filter(toxin_occurance > 5)

data_clean <- data_clean %>% 
  select(c("Snake", "Reference", "Region", summed_toxins$toxin))


# Write cleaned data
data_clean %>% 
  write_csv('data/02_data_clean.csv')

## Add new data
data_clean <- data_clean %>% 
  rename(`SP (Serine Proteinase)` = "SP (Serine roteinase), %")
         # `α-NTx (α-NeuroToxin)` = "?-NTx (?-NeuroToxin), %")

new_data <- read_csv("data/01.2_new_data.csv")

data_clean_new <- data_clean %>% 
  full_join(new_data, by = c("Snake", "Reference")) %>%
  mutate_each(list(~replace(., which(is.na(.)), 0)))

data_clean_new %>% 
  write_csv('data/02_data_new_clean.csv')
