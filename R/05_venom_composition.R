library(tidyverse)
library(plotly)
source('R/99_proj_func.R')


# Load augmented data
data_aug <- read_csv("data/03_data_aug.csv")

# Store toxin names for plotting
toxin_names <- data_aug %>% 
  select_if(is.numeric) %>% 
  colnames()


# Compare venom composition of the two snake families ----------------------
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
       x = "Mean venom composition (%)")
ggsave("results/05_family_legend.png", plot = family_toxins,
       device = "png", scale = 2)

# Save plot in Rdata file
family_plotly <- ggplotly(family_toxins + theme(legend.position = "none"))
save(family_plotly, file = "results/05_family_plotly.Rdata")


# Which toxin is most abundant for each genus -----------------------------
genus_count <- data_aug %>% 
  count(Genus)
avg_toxin <- data_aug %>% 
  select(Genus, Family, all_of(toxin_names)) %>% 
  group_by(Genus, Family) %>% 
  summarise_all(sum) %>% 
  pivot_longer(-c(Genus, Family), names_to = "Toxin") %>% 
  group_by(Family, Genus) %>%
  filter(value == max(value)) %>% 
  inner_join(genus_count, by = "Genus") %>% 
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
  labs(title = "Average abundance",
       subtitle = "Comparing all genera",
       y = "Average abundance (%)")

ggsave("results/05_avg_toxin_genus.png", plot = avg_toxin, device = "png", width = 9)


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
  labs(title = "Intra species comparison",
       y = "Venom composition (%)")

ggsave("results/05_intra_species.png", plot = intra_species,
       device = "png", scale = 2)
# Save plot in Rdata file
intra_species_plotly <- ggplotly(intra_species + theme(legend.position = "none"))
save(intra_species_plotly, file = "results/05_intra_species.Rdata")


# Compare snake genera ----------------------------------------------------
compare_two <- data_aug %>% 
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
       subtitle = "Viperidae: 'Bothrops atrox', Elapidae: 'Naja kaouthia'" )

ggsave("results/05_compare_two.png", plot = compare_two,
       device = "png", scale = 2)
# Save plot in Rdata file
compare_two_plotly <- ggplotly(compare_two + theme(legend.position = "none")) %>% 
  layout(title = list(text = paste0('Comparing venom composition',
                                    '<br>',
                                    '<sup>',
                                    'Viperidae: "Bothrops atrox", Elapidae: "Naja kaouthia"',
                                    '</sup>')))
save(compare_two_plotly, file = "results/05_compare_two.Rdata")


