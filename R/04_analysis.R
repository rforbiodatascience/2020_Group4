# Analysis of the snake data

library(tidyverse)
source('R/99_proj_func.R')
# 
# Make aggregations, summarizing and comparing toxins between genera or countries
# Comparative plots of these
# Most common toxin families (within region, snake family, overall)

# Compare venom composition within one snake species
# Make plots prettier
# Map country diversity on world map or the like (if possible)

# Load augmented data
data_aug <- read_csv("data/03_data_aug.csv")

# Distribution of genera
data_aug %>% 
  distinct(Snake, Genus) %>% 
  ggplot(aes(y = Genus)) +
  geom_bar()


# Which toxin is most common for each genus
#### Relative count (divide by genus count)
toxins <- data_aug %>% 
  select_if(is.numeric)
data_aug %>% 
  select(Genus, colnames(toxins)) %>% 
  group_by(Genus) %>%
  summarise_all(is_not_zero) %>%
  pivot_longer(-Genus) %>%
  group_by(Genus) %>%
  # count(name) %>% 
  filter(value == max(value))

# Which toxin is most abundant for each genus
#### Relative count (divide by genus count)
data_aug %>% 
  select(Genus, colnames(toxins)) %>% 
  group_by(Genus) %>%
  summarise_all(sum) %>%
  pivot_longer(-Genus) %>%
  group_by(Genus) %>%
  filter(value == max(value))

# Region with most different snakes
data_aug %>% 
  distinct(Region, Snake) %>% 
  count(Region) %>%
  arrange(desc(n))

# Bar chart comparing within snake species
data_aug %>% 
  filter(Snake == "Naja kaouthia") %>%
  mutate(id = paste(Snake, " (", row_number(), ")", sep = "")) %>%
  pivot_longer(colnames(toxins), names_to = "Toxin") %>% 
  mutate(value = round(value, 2)) %>% 
  rename(Value = value) %>% 
  arrange(desc(Snake)) %>% 
  ggplot(aes(x = id, y = Value, fill = Toxin)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  ylab('Venom composition (%)')

library(plotly)

p <- data_aug %>% 
  filter(Snake %in% c("Naja kaouthia", "Bothrops atrox")) %>%
  pivot_longer(colnames(toxins), names_to = "Toxin") %>% 
  # group_by(Snake) %>%
  # summarise(value/sum(value)) %>% View()
  mutate(value = round(value, 2)) %>%
  rename(Value = value) %>%
  arrange(desc(Snake)) %>% 
  ggplot(aes(x = Snake, y = Value, fill = Toxin)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme(legend.position = "none") +
  ylab('Venom composition')

ggplotly(p)


#Compare venom compostion of the two snake families
family_toxins <- data_aug %>% 
  pivot_longer(colnames(toxins), names_to = "Toxin") %>% 
  mutate(value = round(value, 2)) %>%
  rename(Value = value) %>%
  arrange(desc(Family)) %>% 
  ggplot(aes(x = Family, y = Value, fill = Toxin)) +
  geom_col(position = "fill") +
  coord_flip() +
  #theme(legend.position = "none") +
  ylab('Venom composition (%)')

ggsave(filename = 'results/04_family_toxins.png', family_toxins, 
       scale = 1.8)
