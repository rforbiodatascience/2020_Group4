library(tidyverse)
source('R/99_proj_func.R')


# Load raw data -----------------------------------------------------------
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")


###### Clean note column and add new column containing grouped regions
Brazilian_cities <- c("Juazeiro", "Ceara", "Paraiba", "Pernambuco", "ilha de Itaparica", "Adult and young in Brazil")
USA <- c("Colorado", "Arizona", "Idyllwild", "Loma Linda", "Phelan", "Catalina", "Texas", "Kentucky", "Missouri",
         "Florida", "Kansas", "Colorado", "Ohio", "New Mexico")

data_clean <- data_raw %>% 
  rename(`SP (Serine Proteinase), %` = `SP (Serine roteinase), %`,
         `α-NTx (α-NeuroToxin), %` = `?-NTx (?-NeuroToxin), %`) %>% 
  filter(!(str_to_lower(Note) %in% c("origin unknown", "pooled", "neonate", "adult")),
         # Condition in data set is that rownames ending with * indicates transcriptomic data
         str_detect(Snake, '\\*', negate = TRUE)) %>% 
  mutate(Region = case_when(
                            detect_in_list(Note, USA) ~ "USA",
                            Note %in% Brazilian_cities ~ "Brazil",
                            str_detect(Note, "Caribbean") ~ "Caribbean",
                            str_detect(Note, "Pacific") ~ "Pacific",
                            str_detect(str_to_lower(Note), "costa rica") ~ "Costa Rica",
                            str_detect(Note, "Venezuelan") ~ "Venezuela",
                            str_detect(Note, "Mexican") ~ "Mexico",
                            str_detect(Note, 'India') ~ "India",
                            Note == "Carribean" ~ "Caribbean",
                            Note == "Columbia" ~ "Colombia",
                            Note == "Marocco" ~ "Morocco",
                            Note == "Tunesia" ~ "Tunisia",
                            Note == "Woodlark island" ~ "Papua New Guinea",
                            Note == "Saibai Island" ~ "Australia",
                            Note == "Java Island" ~ "Indonesia",
                            Note == "Chinese adult" ~ "China",
                            TRUE ~ Note)) %>% 
  select(-c("Note", "Sum, %"))





# Remove % from toxin names ------------------------------------
data_clean <- data_clean %>%
 rename_if(is_double, rm_percent)





# Add new data ------------------------------------------------------------
new_data <- read_csv("data/01.2_new_data.csv")

data_clean_new <- data_clean %>% 
  full_join(new_data) %>%
  mutate_each(list(~replace(., which(is.na(.)), 0)))

data_clean_new %>% 
  write_csv('data/02_data_clean.csv')
