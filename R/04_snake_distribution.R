library(tidyverse)
library(plotly)
library(maps)

# Load augmented data
data_aug <- read_csv("data/03_data_aug.csv")

# World map ---------------------------------------------------------------
# Country with most different snakes
data_world <- data_aug %>% 
  distinct(Country, Snake) %>% 
  count(Country) %>%
  arrange(desc(n))

n_unknown <- data_world %>%
  filter(Country == "Unknown") %>%
  pull(n)

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

world_plotly <- ggplotly(world) %>% 
  layout(title = list(text = paste0("World map of snake counts",
                                    '<br>',
                                    '<sup>',
                                    paste("There are", n_unknown, "snakes of unknown origin."),
                                    '</sup>')))

save(world_plotly, file = "results/04_world_of_snakes.Rdata")

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