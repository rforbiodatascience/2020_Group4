library(tidyverse)
library(plotly)
library(maps)
source('R/99_proj_func.R')

# Store plotly files in results folder
results_dir <- paste0(getwd(),"/results")

# Load augmented data -----------------------------------------------------
data_aug <- read_csv("data/03_data_aug.csv")

# Distribution of genera --------------------------------------------------
data_aug %>% 
  distinct(Snake, Genus, Family) %>% 
  ggplot(aes(y = Genus, fill = Family)) +
  labs(x = "Count", title = "Count of distinct snakes in genera") +
  geom_bar()


# Most common toxins for each genus ---------------------------------------
toxins <- data_aug %>% 
  select_if(is.numeric)
data_aug %>% 
  select(Snake, Genus, colnames(toxins)) %>% 
  group_by(Genus) %>% 
  summarise_all(is_not_zero) %>% 
  filter(Genus %in% c("Bothrops")) %>%
  pivot_longer(-c(Genus, Snake)) %>% 
  arrange(desc(value)) %>% 
  ggplot(aes(x = value, y = name, fill = value)) +
  geom_col() + 
  labs(y = "Toxins", title = "Abundance of toxins in 'Bothrops'")


# Which toxin is most abundant for each genus -----------------------------

genus_num <- data_aug %>% 
  count(Genus)
data_aug %>% 
  select(Genus, Family, colnames(toxins)) %>% 
  group_by(Genus, Family) %>% 
  summarise_all(sum) %>% 
  pivot_longer(-c(Genus, Family), names_to = "Toxin") %>% 
  group_by(Family, Genus) %>% 
  filter(value == max(value)) %>% 
  inner_join(genus_num, by = "Genus") %>% 
  mutate(avg_abundance = round(value / n, 2)) %>% 
  ggplot(aes(x = avg_abundance, y = Genus, fill = Toxin, color = Family)) +
  geom_col(width = 0.7) +
  labs(title = "Average abundance",
       subtitle = "Comparing all genera",
       x = "Average abundance (%)")
ggsave("results/04_avg_toxin_genus.png", device = "png")

# Country with most different snakes --------------------------------------
data_world <- data_aug %>% 
  distinct(Country, Snake) %>% 
  count(Country) %>%
  arrange(desc(n))


# World map ---------------------------------------------------------------
map.world <- map_data("world")

map.world_joined <- map.world %>% 
  left_join(data_world, by = c('region' = 'Country')) %>% 
  rename(count = n)

world <- map.world_joined %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = count, label = region)) +
    geom_polygon() +
    scale_fill_gradient(
      low = "white",
      high = "red"
    ) +
    labs(title = "World map of snake counts",
         x = "Longitude",
         y = "Latitude",
         fill = "Snake count")
ggsave(filename = paste0(results_dir, "/04_world_of_snakes.png"), device = "png")

# Bar chart comparing within snake species --------------------------------
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


# Compare snake genera ----------------------------------------------------
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
  labs(y = 'Venom composition (%)', 
       title = "Comparing venom composition",  
       subtitle = "Viperidae: 'Bothrops atrox', Elapidae: 'Naja kaouthia'" ) +

  coord_flip() +
  theme(legend.position = "none") #+
  #ylab('Venom composition')

ggplotly(p) %>%
  layout(title = list(text = paste0('Comparing venom composition',
                                    '<br>',
                                    '<sup>',
                                    'Viperidae: "Bothrops atrox", Elapidae: "Naja kaouthia"',
                                    '</sup>')))



# Compare venom compostion of the two snake families ----------------------
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
  labs(title = "Mean Venom Composition of Viperidae and Elapidae",
       y = "Mean venom composition (%)") +
  theme(legend.position = "none")

# Save plot in html file
family_plotly <- ggplotly(family_toxins)
htmlwidgets::saveWidget(family_plotly, file = paste0(results_dir, "/04_family_plotly.html"))
