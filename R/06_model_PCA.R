library(broom)
library(ggplot2)
library(tidyverse)

###### Load augmented data
data <- read_csv("data/03_data_aug.csv")  

###### PCA
data_new <- data %>%
  select_if(is.numeric)

#Create PCA object
data_pca <- data_new %>%
  prcomp(center = TRUE, scale. = TRUE)

#Scree plot using broom to tidy
data_pca %>% 
  tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

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
             colour = family)) +
  geom_point() + 
  labs(x = x, y = y)


###### K-means
data_k_org <- data_pca_aug %>%
  select(contains("PC")) %>% 
         # contains("PLB")) %>%
  #select(as.character(.[7:80])) %>%
  kmeans(centers = 2)
data_k_org

data_pca_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = data_k_org$cluster)) +
  geom_point()

