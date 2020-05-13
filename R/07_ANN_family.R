# This script is heavily inspired by the course material given in the course 
# 22100 - R for Bio Data Science at the Technical University of Denmark.
# http://teaching.healthtech.dtu.dk/22100/index.php/22100_-_R_for_Bio_Data_Science

# Only run first time:
# source('R/07_ANN_setup.R')

rm(list=ls())

library('tidyverse')
library('keras')

# sample(1e6,1)
set.seed(656907)

# Load augmented data, and filter for low observations
data_aug <- read_csv("data/03_data_aug.csv")  

#  Load data into test and training ---------------------------------------

nn_data <- data_aug %>%
  # Add family labels and factors
  mutate(class_label = as.factor(Family),
         class_num = as.numeric(class_label) - 1 # factor, so = 0, 1
         ) %>%
  # Reorganise order of columns
  select(1:Reference, class_label, class_num, everything(), -Unknown)

# Partition into test and train sets
test_f <- 0.25
nn_data <- nn_data %>%
  mutate(partition = sample(x = c('train','test'),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))
# Partition distribution
nn_data %>% count(partition)

x_train <- nn_data %>%
  filter(partition == 'train') %>%
  select_if(is.numeric) %>%
  select(-class_num) %>%
  as.matrix

y_train <- nn_data %>%
  filter(partition == 'train') %>%
  pull(class_num) %>%
  to_categorical

x_test <- nn_data %>%
  filter(partition == 'test') %>%
  select_if(is.numeric) %>%
  select(-class_num) %>%
  as.matrix

y_test <- nn_data %>%
  filter(partition == 'test') %>%
  pull(class_num) %>%
  to_categorical



# Model -------------------------------------------------------------------

# Number of features is number of input venoms
n_features <- ncol(x_train)

# Number of classes is the number of snake families investigated (2)
n_classes <- nn_data %>% 
  distinct(Family) %>%
  count() %>%
  pull(n)

# Define training parameters
model <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = n_features) %>% 
  layer_dense(units = n_classes, activation = 'softmax')


# Compile model architecture
model %>%
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_rmsprop(lr = 0.005),
          metrics = c('accuracy')
  )

# Train model
history <- model %>%
  fit(x = x_train,
      y = y_train,
      epochs = 100,
      batch_size = 50,
      validation_split = 0.20
  )

# Save history as png
png('results/07_ANN_family_training.png', width = 500, height = 500)
plot(history)
dev.off()

performance <- model %>%
  evaluate(x_test, y_test)

# Make predictions on test set
plot_dat <- nn_data %>%
  filter(partition == 'test') %>%
  mutate(class_num = as.factor(class_num),
         y_pred = as.factor(predict_classes(model, x_test)),
         Correct = as.factor(ifelse(class_num == y_pred, "Yes", "No")))


accuracy_plot <- plot_dat %>% 
  ggplot(aes(x = class_num, y = y_pred, colour = Correct)) + 
  geom_jitter() + 
  scale_x_discrete(labels = levels(nn_data$class_label)) +  
  scale_y_discrete(labels = levels(nn_data$class_label)) +     
  theme_bw() + 
  labs(title = "Classification Performance of Artificial Neural Network", 
       subtitle = str_c("Accuracy = ", round(performance$acc, 3) * 100, "%"),
       x = "True snake family",
       y = "Predicted snake family")

ggsave("results/05_accuracy_plot.png", plot = accuracy_plot, device = "png")



# Create table with mislabeled snakes -------------------------------------------

incorrect <- plot_dat %>% 
  filter(Correct == "No")

write_csv(incorrect, "results/05_incorrect_pred.csv")
