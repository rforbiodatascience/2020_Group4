library(broom)
library(ggplot2)
library(tidyverse)

###### Load augmented data
data <- read_csv("data/03_data_aug.csv")  


# PCA ---------------------------------------------------------------------

#Create PCA object
data_pca <- data %>%
  select_if(is.numeric) %>% 
  select(-Unknown) %>%
  prcomp(center = TRUE, scale. = TRUE)

#Scree plot using broom to tidy
data_pca %>% 
  tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_grey()

#Augment
data_pca_aug <- data_pca %>% 
  augment(data)

#Adding percentage to the PCA plot
x <- data_pca %>% 
  tidy("pcs") %>% 
  filter(PC==1) %>% 
  pull(percent)
x <- str_c("PC1 (", round(x*100, 2), "%)")

y <- data_pca %>% 
  tidy("pcs") %>% 
  filter(PC==2) %>% 
  pull(percent)
y <- str_c("PC2 (", round(y*100, 2), "%)")

#Plot PCA
data_pca_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = Family)) +
  geom_point() + 
  labs(x = x, y = y, title = "Plot of PCA", color = "Snake family") +
  theme_grey()



# K-means -----------------------------------------------------------------

data_k_org <- data_pca_aug %>%
  select(contains("PC")) %>% 
  kmeans(centers = 2)

data_pca_aug_k_org <- data_k_org %>%
  augment(data_pca_aug) %>% 
  rename(cluster_org = .cluster)

data_pca_aug_k_org %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             color = cluster_org)) +
  geom_point() +
  labs(x = x, y = y, title = "Plot of k-means", color = "Snake family")

