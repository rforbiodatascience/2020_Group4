rm(list = ls())

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
  summarise(Value = Value %>% 
              mean() %>%
              round(2)) %>% 
  ggplot(aes(x = Value, y = Family, fill = Toxin)) +
  geom_col() +
  labs(title = "Mean Venom Composition of Viperidae and Elapidae",
       x = "Mean venom composition (%)")

# Save plot as png and in Rdata file
ggsave("results/05_family_legend.png", plot = family_toxins,
       device = "png", scale = 1.5, width = 6.17, height = 3.1)

family_plotly <- ggplotly(family_toxins + theme(legend.position = "none"))
save(family_plotly, file = "results/05_family_plotly.Rdata")


# Which toxin is most abundant for each genus -----------------------------

avg_toxin <- data_aug %>% 
  select(Genus, Family, all_of(toxin_names)) %>% 
  group_by(Genus, Family) %>% 
  summarise_all(mean) %>% 
  pivot_longer(-c(Genus, Family), names_to = "Toxin", values_to = "toxin_amount") %>% 
  filter(toxin_amount == max(toxin_amount)) %>% 
  ggplot(aes(x = Genus, y = toxin_amount, fill = Toxin)) +
  geom_col(width = 0.7) +
  facet_grid(cols = vars(Family),
             space = "free",
             scales = "free") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.4)) +
  labs(title = "Primary toxin in each genus",
       subtitle = "Segregated by family",
       y = "Average abundance (%)")

# Save plot as png
ggsave("results/05_avg_toxin_genus.png", plot = avg_toxin, device = "png", width = 6.17, height = 3.1)


# Intra species comparison --------------------------------
intra_species <- data_aug %>% 
  filter(Snake == "Naja kaouthia") %>%
  mutate(Snake = paste0(Snake, " (",
                       row_number(), ")")
         ) %>%
  pivot_longer(all_of(toxin_names),
               names_to = "Toxin",
               values_to = "Toxin amount (%)") %>% 
  ggplot(aes(x = `Toxin amount (%)`, y = Snake, fill = Toxin)) +
  geom_col() +
  labs(title = "Intra species comparison of Naja kaouthia",
       x = "Venom composition (%)")

ggsave("results/05_intra_species.png", plot = intra_species,
       device = "png", scale = 1.5, width = 6.17, height = 3.1)
# Save plot in Rdata file
intra_species_plotly <- ggplotly(intra_species + theme(legend.position = "none"))
save(intra_species_plotly, file = "results/05_intra_species.Rdata")


# Compare snake genera ----------------------------------------------------
compare_two <- data_aug %>% 
  filter(Snake %in% c("Naja kaouthia", "Daboia russelii russelii")) %>% 
  pivot_longer(all_of(toxin_names),
               names_to = "Toxin",
               values_to = "toxin_amount") %>% 
  group_by(Snake, Toxin) %>%
  summarise(`Toxin amount (%)` = toxin_amount %>% 
              mean() %>%
              round(2)) %>% 
  ggplot(aes(x = `Toxin amount (%)`, y = Snake, fill = Toxin)) +
  geom_col() +
  labs(x = 'Venom composition (%)', 
       title = "Comparing venom composition",  
       subtitle = "Viperidae: 'Daboia russelii russelii', Elapidae: 'Naja kaouthia'" )

ggsave("results/05_compare_two.png", plot = compare_two,
       device = "png", scale = 1.5, width = 6.17, height = 3.1)
# Save plot in Rdata file
compare_two_plotly <- ggplotly(compare_two + theme(legend.position = "none")) %>% 
  layout(title = list(text = paste0('Comparing venom composition',
                                    '<br>',
                                    '<sup>',
                                    'Viperidae: "Daboia russelii russelii", Elapidae: "Naja kaouthia"',
                                    '</sup>')))
save(compare_two_plotly, file = "results/05_compare_two.Rdata")


