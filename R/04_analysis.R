# Analysis of the snake data

library(tidyverse)
source('R/99_proj_func.R')
# 
# Make aggregations, summarizing and comparing toxins between genera or countries
# Comparative plots of these
# Most common toxin families (within region, snake family, overall)

# Load augmented data
data_aug <- read_csv("data/03_data_aug.csv")

# Distribution of genera
data_aug %>% 
  distinct(Snake, genus) %>% 
  ggplot(aes(y = genus)) +
  geom_bar()


# Which toxin is most common for each genus
toxins <- data_aug %>% 
  select_if(is.numeric)
data_aug %>% 
  select(genus, colnames(toxins)) %>% 
  group_by(genus) %>%
  summarise_all(is_not_zero) %>%
  pivot_longer(-genus) %>%
  group_by(genus) %>%
  # count(name) %>% 
  filter(value == max(value)) %>%
  View()

# Which toxin is most abundant for each genus
data_aug %>% 
  select(genus, colnames(toxins)) %>% 
  group_by(genus) %>%
  summarise_all(sum) %>%
  pivot_longer(-genus) %>%
  group_by(genus) %>%
  filter(value == max(value)) %>%
  View()

# Region with most different snakes
data_aug %>% 
  distinct(Region, Snake) %>% 
  count(Region) %>%
  arrange(desc(n)) %>% 
  View()

