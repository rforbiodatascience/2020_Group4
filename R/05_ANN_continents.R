#source('R/05_ANN_setup.R')

rm(list=ls())

library('tidyverse')
library('keras')

###### Load augmented data, and filter for low observations
data <- read_csv("data/03_data_aug.csv")  


##### Load data into test and training

nn_dat <- data %>%
  #Add Continent labels and factors
  mutate(class_num = as.numeric(as.factor(Continent)) - 1, # factor, so = 0, 1
         class_label = as.factor(Continent)) %>%
  #Reorganise order of columns
  select(1:Continent, class_label, class_num, everything())
nn_dat %>% head(3)

test_f <- 0.20
nn_dat <- nn_dat %>%
  mutate(partition = sample(x = c('train','test'),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))
nn_dat %>% count(partition)

#Get count of different classes (data is biased towards America)
nn_dat %>% 
  group_by(class_label) %>% 
  count()


# Merge continents --------------------------------------------------------

#Due to the small amount of samples from all countries but America, these continents are gathered into one.

Not_America <- c("Africa", "Eurasia", "Oceania")

nn_dat <- nn_dat %>% 
  mutate(Continent = case_when(
    Continent %in% Not_America ~ "Not_America",
    TRUE ~ Continent))

nn_dat <- nn_dat %>%
  #Add Continent labels and factors
  mutate(class_num = as.numeric(as.factor(Continent)) - 1, # factor, so = 0, 1
         class_label = as.factor(Continent)) %>%
  #Reorganise order of columns
  select(1:Continent, class_label, class_num, everything())
nn_dat %>% head(3)

test_f <- 0.20
nn_dat <- nn_dat %>%
  mutate(partition = sample(x = c('train','test'),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))
nn_dat %>% count(partition)

#Get count of different classes (data is biased towards America)
nn_dat %>% 
  group_by(class_label) %>% 
  count()

# Create training and test data -------------------------------------------

x_train <- nn_dat %>%
  filter(partition == 'train') %>%
  select_if(is.numeric) %>%
  select(-class_num) %>%
  as.matrix()

y_train <- nn_dat %>%
  filter(partition == 'train') %>%
  pull(class_num) %>%
  to_categorical

x_test <- nn_dat %>%
  filter(partition == 'test') %>%
  select_if(is.numeric) %>%
  select(-class_num) %>%
  as.matrix
y_test <- nn_dat %>%
  filter(partition == 'test') %>%
  pull(class_num) %>%
  to_categorical


#### OLD MODEL:

#Number of features is number of input venoms (38)
n_features = ncol(x_train)

#Number of classes is the number of snake families investigated (2) (n_output)
n_classes = length(unique(nn_dat$Continent))

# Define ANN model
# ------------------------------------------------------------------------------

# Set hyperparameters
n_hidden_1 <- 12
h1_activate <- 'relu'
drop_out_1 <- 0.1
n_hidden_2 <- 4
h2_activate <- 'relu'
drop_out_2 <- 0.1
o_ativate  <- 'softmax'
n_epochs <- 100
batch_size <- 30
loss_func <- 'binary_crossentropy'
# loss_func <- 'categorical_crossentropy'
learn_rate <- 0.005

# Set architecture
model = keras_model_sequential() %>% 
  layer_dense(units = n_hidden_1, activation = h1_activate, input_shape = n_features) %>% 
  layer_dropout(rate = drop_out_1) %>% 
  layer_dense(units = n_hidden_2, activation = h2_activate) %>%
  layer_dropout(rate = drop_out_2) %>%
  layer_dense(units = n_classes, activation = o_ativate)

# Compile model
model %>%
  compile(loss = loss_func,
          optimizer = optimizer_rmsprop(lr = learn_rate),
          metrics = c('accuracy')
  )

# View model
model %>% summary %>% print

# Train model
# ------------------------------------------------------------------------------
history = model %>%
  fit(x = x_train,
      y = y_train,
      epochs = n_epochs,
      batch_size = batch_size,
      validation_split = 0.2
  )


plot(history)


perf <- model %>% evaluate(x_test, y_test)
perf

plot_dat <- nn_dat %>%
  filter(partition == 'test') %>%
  mutate(class_num = factor(class_num),
         y_pred = factor(predict_classes(model, x_test)),
         Correct = factor(ifelse(class_num == y_pred, "Yes", "No")))
plot_dat %>% select(-contains("feat")) %>% head(3)


title     = "Classification Performance of Artificial Neural Network"
sub_title = str_c("Accuracy = ", round(perf$acc, 3) * 100, "%")
x_lab     = "True snake Continent"
y_lab     = "Predicted snake Continent"
accuracy_plot <- plot_dat %>% ggplot(aes(x = class_num, y = y_pred, colour = Correct)) +
  geom_jitter() +
  scale_x_discrete(labels = levels(nn_dat$class_label)) +
  scale_y_discrete(labels = levels(nn_dat$class_label)) +
  theme_bw() +
  labs(title = title, subtitle = sub_title, x = x_lab, y = y_lab)

