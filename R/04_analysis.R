# Analysis of the snake data

library(tidyverse)
# 
# Make aggregations, summarizing and comparing toxins between genera or countries
# Comparative plots of these
# Most common toxin families (within region, snake family, overall)

# Load augmented data
data_aug <- read_csv("data/03_relative_aug.csv")

# Distribution of genera
data_aug %>% 
  ggplot(aes(y = genus)) +
  geom_bar()


# Which toxin is most common for each genus
data_aug %>% 
  group_by(genus) %>% 
  summarise(n()) %>% 
  View()


