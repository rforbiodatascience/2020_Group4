# Analysis of the snake data

library(tidyverse)
library(plotly)
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

# Country with most different snakes
data_aug %>% 
  distinct(Country, Snake) %>% 
  count(Country) %>%
  arrange(desc(n))

# Bar chart comparing within snake species
p <- data_aug %>% 
  filter(Snake == "Naja kaouthia") %>%
  mutate(Snake = paste(Snake, " (",
                       row_number(), ")",
                       sep = "")) %>%
  pivot_longer(colnames(toxins),
               names_to = "Toxin",
               values_to = "Value") %>% 
  # mutate(Value = round(Value, 2)) %>% 
  arrange(desc(Snake)) %>% 
  ggplot(aes(x = Snake, y = Value, fill = Toxin)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  ylab('Venom composition (%)')
ggplotly(p)

# Compare snake genera
p <- data_aug %>% 
  filter(Snake %in% c("Naja kaouthia", "Bothrops atrox")) %>%
  pivot_longer(colnames(toxins),
               names_to = "Toxin",
               values_to = "Value") %>% 
  group_by(Snake, Toxin) %>%
  summarise(mean(Value)) %>%
  mutate(Value = round(`mean(Value)`, 2)) %>%
  filter(Value > 0) %>%
  ggplot(aes(x = Snake, y = Value, fill = Toxin)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  ylab('Venom composition')

ggplotly(p)


#Compare venom compostion of the two snake families
family_toxins <- data_aug %>% 
  pivot_longer(colnames(toxins),
               names_to = "Toxin",
               values_to = "Value") %>% 
  group_by(Family, Toxin) %>%
  summarise(mean(Value)) %>%
  mutate(Value = round(`mean(Value)`, 2)) %>%
  ggplot(aes(x = Family, y = Value, fill = Toxin)) +
  geom_col() +
  coord_flip() +
  labs(title = "Venom composition of vipers and elapids",
       subtitle = "Mean venom composition of the two snake families") +
  theme(legend.position = "none")

ggplotly(family_toxins)
# ggsave(filename = 'results/04_family_toxins.png', family_toxins, 
       # scale = 1.8)
