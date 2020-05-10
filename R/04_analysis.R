library(tidyverse)
library(plotly)
library(maps)
source('R/99_proj_func.R')

# Store plotly files in results folder
results_dir <- paste0(getwd(), "/results")

# Load augmented data -----------------------------------------------------
data_aug <- read_csv("data/03_data_aug.csv")
# Store toxin names for easier plotting
toxin_names <- data_aug %>% 
  select_if(is.numeric) %>% 
  colnames()


# World map ---------------------------------------------------------------
# Country with most different snakes
data_world <- data_aug %>% 
  distinct(Country, Snake) %>% 
  count(Country) %>%
  arrange(desc(n))

map.world <- map_data("world")

map.world_joined <- map.world %>% 
  left_join(data_world, by = c('region' = 'Country')) %>% 
  rename(count = n)

world <- map.world_joined %>% 
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = count,
             label = region)) +
  geom_polygon() +
  scale_fill_gradient(low = "#ffded2",
                      high = "red") +
  labs(title = "World map of snake counts",
       x = "Longitude",
       y = "Latitude",
       fill = "Snake count")
# Too big to be stored as html
ggsave(filename = "results/04_world_of_snakes.png", device = "png")



# Distribution of genera --------------------------------------------------
genus_count <- data_aug %>% 
  distinct(Snake, Genus, Family) %>% 
  ggplot(aes(x = Genus, fill = Family)) +
  geom_bar() + 
  facet_grid(. ~ Family,
             space = "free",
             scales = "free") +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4),
        legend.position = "none") +
  labs(y = "Count", title = "Count of distinct snakes in genera")
ggsave("results/04_genus_distribution.png", plot = genus_count, device = "png")


# Compare venom compostion of the two snake families ----------------------
family_toxins <- data_aug %>% 
  pivot_longer(all_of(toxin_names),
               names_to = "Toxin",
               values_to = "Value") %>% 
  group_by(Family, Toxin) %>%
  summarise(mean(Value)) %>%
  mutate(Value = round(`mean(Value)`, 2)) %>%
  ggplot(aes(x = Value, y = Family, fill = Toxin)) +
  geom_col() +
  labs(title = "Mean Venom Composition of Viperidae and Elapidae",
       x = "Mean venom composition (%)") +
  theme(legend.position = "none")

# Save plot in html file
family_plotly <- ggplotly(family_toxins)
htmlwidgets::saveWidget(family_plotly, file = paste0(results_dir, "/04_family_plotly.html"))


# Which toxin is most abundant for each genus -----------------------------
genus_num <- data_aug %>% 
  count(Genus)
data_aug %>% 
  select(Genus, Family, all_of(toxin_names)) %>% 
  group_by(Genus, Family) %>% 
  summarise_all(sum) %>% 
  pivot_longer(-c(Genus, Family), names_to = "Toxin") %>% 
  group_by(Family, Genus) %>%
  filter(value == max(value)) %>% 
  inner_join(genus_num, by = "Genus") %>% 
  mutate(avg_abundance = round(value / n, 2)) %>%
  ggplot(aes(x = Genus, y = avg_abundance, fill = Toxin)) +
  geom_col(width = 0.7) +
  facet_grid(. ~ Family,
             space = "free",
             scales = "free") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4)) +
  # scale_fill_brewer(palette="Set1") +
  # scale_colour_manual(values = c("#f8766d", "#00b7e9", "#b584ff", "#ec68ee", "#f663e1"), aesthetics = "fill") +
  labs(title = "Average abundance",
       subtitle = "Comparing all genera",
       y = "Average abundance (%)")

ggsave("results/04_avg_toxin_genus.png", device = "png")


# Intra species comparizon --------------------------------
intra_species <- data_aug %>% 
  filter(Snake == "Naja kaouthia") %>%
  mutate(Snake = paste(Snake, " (",
                       row_number(), ")",
                       sep = "")) %>%
  pivot_longer(all_of(toxin_names),
               names_to = "Toxin",
               values_to = "Value") %>% 
  arrange(desc(Snake)) %>% 
  ggplot(aes(x = Snake, y = Value, fill = Toxin)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  ylab('Venom composition (%)')

intra_species <- ggplotly(intra_species)
htmlwidgets::saveWidget(intra_species, file = paste0(results_dir, "/04_intra_species.html"))


# Compare snake genera ----------------------------------------------------
compareTwo <- data_aug %>% 
  filter(Snake %in% c("Naja kaouthia", "Bothrops atrox")) %>%
  pivot_longer(all_of(toxin_names),
               names_to = "Toxin",
               values_to = "Value") %>% 
  group_by(Snake, Toxin) %>%
  summarise(mean(Value)) %>%
  mutate(Value = round(`mean(Value)`, 2)) %>%
  filter(Value > 0) %>%
  ggplot(aes(x = Value, y = Snake, fill = Toxin)) +
  geom_col() +
  labs(x = 'Venom composition (%)', 
       title = "Comparing venom composition",  
       subtitle = "Viperidae: 'Bothrops atrox', Elapidae: 'Naja kaouthia'" ) +
  theme(legend.position = "none") #+

compareTwo <- ggplotly(compareTwo) %>%
  layout(title = list(text = paste0('Comparing venom composition',
                                    '<br>',
                                    '<sup>',
                                    'Viperidae: "Bothrops atrox", Elapidae: "Naja kaouthia"',
                                    '</sup>')))
htmlwidgets::saveWidget(compareTwo, file = paste0(results_dir, "/04_compareTwo.html"))


