# This script is heavily inspired by the course material given in the course 
# 22100 - R for Bio Data Science at the Technical University of Denmark.
# http://teaching.healthtech.dtu.dk/22100/index.php/22100_-_R_for_Bio_Data_Science


rm(list = ls())

library(broom)
library(tidyverse)
library(patchwork)

# sample(1e6, 1)
set.seed(839865)


# Load augmented data
data_aug <- read_csv("data/03_data_aug.csv")  

# PCA ---------------------------------------------------------------------

# Create PCA object
data_pca <- data_aug %>%
  select_if(is.numeric) %>% 
  select(-Unknown) %>%
  prcomp(scale. = TRUE)

# Tidy
data_pca_tidy <- data_pca %>% 
  tidy("pcs")

# Scree plot using broom to tidy
scree <- data_pca_tidy %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col()
ggsave("results/06_scree.png", plot = scree, device = "png", width = 6.17, height = 3.1)

# Augment
data_pca_aug <- data_pca %>% 
  augment(data_aug)

# Adding percentage to the PCA plot
x <- data_pca_tidy %>% 
  filter(PC == 1) %>% 
  pull(percent)
x <- str_c("PC1 (", round(x*100, 2), "%)")

y <- data_pca_tidy %>% 
  filter(PC == 2) %>% 
  pull(percent)
y <- str_c("PC2 (", round(y*100, 2), "%)")

# Plot PCA with snake family as labels
family_pca <- data_pca_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = Family)) +
  geom_point(shape = 1, size = 3, alpha = 0.5) + 
  labs(x = x, y = y, title = "PCA", color = "Snake family")

# Save plot as png
ggsave("results/06_family_pca.png", plot = family_pca, device = "png", width = 6.17, height = 3.1)

# K-means -----------------------------------------------------------------

# Perform kmeans
data_kmeans <- data_pca_aug %>%
  select(contains("PC")) %>% 
  kmeans(centers = 3)

# Add cluster column to augmented pca table
data_kmeans_aug <- data_kmeans %>%
  augment(data_pca_aug) %>%
  rename(Cluster = .cluster)

# Plot kmeans on two first principal components
kmeans <- data_kmeans_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = Cluster)) +
  geom_point(shape = 1, size = 3, alpha = 0.5) +
  labs(x = x, y = y, title = "K-means")

# Save plot as png
ggsave("results/06_kmeans.png", device = "png", width = 6.17, height = 3.1)


# Save PCA and K-means as multi-panel plot
ggsave("results/06_kmeans-family.png", plot = family_pca + kmeans, device = "png", width = 6.17, height = 3.1)


# Save snake in cluster 2
cluster2 <- data_pca_aug %>% 
  filter(Cluster == 2)
save(cluster2, file = "results/06_kmeans_cluster2.Rdata")
