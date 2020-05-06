library(broom)
library(ggplot2)

###### Load augmented data
data <- read_csv("data/03_data_aug.csv")  

###### PCA
data_new <- data %>%
  select_if(is.numeric)

data_pca <- data_new %>%
  prcomp(center = TRUE, scale. = TRUE)

data_pca %>% 
  tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

data_pca_aug <- data_pca %>%
  augment(data)

data_pca_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = family)) +
  geom_point()

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
