library(tidyverse)
source('R/99_proj_func.R')

# Load raw data -----------------------------------------------------------
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")


# Clean column containing regions -----------------------------------------
Brazilian_cities <- c("Juazeiro", "Ceara", "Paraiba", "Pernambuco", "ilha de Itaparica", "Adult and young in Brazil")
USA <- c("Colorado", "Arizona", "Idyllwild", "Loma Linda", "Phelan", "Catalina", "Texas", "Kentucky", "Missouri",
         "Florida", "Kansas", "Colorado", "Ohio", "New Mexico", "Forida")

data_clean <- data_raw %>% 
  rename_if(is_double, rm_percent) %>% 
  rename(`SP (Serine Proteinase)` = `SP (Serine roteinase)`,
         `α-NTx (α-NeuroToxin)` = `?-NTx (?-NeuroToxin)`) %>% 
  filter(!(str_to_lower(Note) %in% c("origin unknown", "pooled", "neonate", "adult")),
         # Condition in data set is that rownames ending with * indicates transcriptomic data
         str_detect(Snake, '\\*', negate = TRUE)) %>% 
  mutate(Country = case_when(
                            detect_in_list(Note, USA) ~ "USA",
                            Note %in% Brazilian_cities ~ "Brazil",
                            str_detect(Note, "Caribbean") ~ "Costa Rica",
                            str_detect(Note, "Pacific") ~ "Costa Rica",
                            str_detect(str_to_lower(Note), "costa rica") ~ "Costa Rica",
                            str_detect(Note, "Venezuelan") ~ "Venezuela",
                            str_detect(Note, "Mexican") ~ "Mexico",
                            str_detect(Note, 'India') ~ "India",
                            Note == "Carribean" ~ "Costa Rica",
                            Note == "Columbia" ~ "Colombia",
                            Note == "Marocco" ~ "Morocco",
                            Note == "Tunesia" ~ "Tunisia",
                            Note == "New Guinea" ~ "Papua New Guinea",
                            Note == "Woodlark island" ~ "Papua New Guinea",
                            Note == "Saibai Island" ~ "Australia",
                            Note == "Java Island" ~ "Indonesia",
                            Note == "Chinese adult" ~ "China",
                            Note == "Burkina faso" ~ "Burkina Faso",
                            Note == "North Africa" ~ "Egypt",
                            TRUE ~ Note)) %>% 
  select(-c("Note", "Sum"))

# Add new data ------------------------------------------------------------
new_data <- read_csv("data/_raw/01.2_new_data.csv")

data_joined <- data_clean %>% 
  full_join(new_data) %>%
  mutate_each(list(~replace(., which(is.na(.)), 0)))


# Add Continent column ----------------------------------------------------
America <- c("Argentina", "Bolivia", "Brazil", "Colombia", "Ecuador", 
             "Guyana", "Peru", "Venezuela", "USA", "Burkina faso", 
             "Caribbean", "Costa Rica", "Guatemala", "Honduras", "Mexico", "Pacific")
Eurasia <- c("France", "Russia", "Croatia", "China", "Malaysia", "Vietnam", "Taiwan", "Thailand", "Armenia", "India", "Iran", "Pakistan", "Sri Lanka", "Turkey")
Oceania <- c("Australia", "Indonesia", "New Guinea", "Papua New Guinea")
Africa <- c("Ghana", "Kenya", "Morocco", "Nigeria", "North Africa", "Tanzania", "Tunisia", "Uganda")

data_joined <- data_joined %>% 
  mutate(Continent = case_when(
    Country %in% America ~ "America",
    Country %in%  Eurasia ~ "Eurasia",
    Country %in% Oceania ~ "Oceania",
    Country %in%  Africa ~ "Africa",
    TRUE ~ Country))

# Write output clean file -------------------------------------------------
data_joined %>% 
  write_csv('data/02_data_clean.csv')
