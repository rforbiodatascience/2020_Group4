#install.packages("keras")
library('tidyverse')
library('keras')


###### Load augmented data
data <- read_csv("data/03_relative_aug.csv")    


nn_dat = data %>% as_tibble %>%
  #Add genus labels and factors
  mutate(class_num = as.numeric(as.factor(genus)) - 1, # factor, so = 0, 1, 2
         class_label = as.factor(genus)) %>%
  #Reorganise order of columns
  select(1:Note, class_label, class_num, everything())
nn_dat %>% head(3)


test_f = 0.20
nn_dat = nn_dat %>%
  mutate(partition = sample(x = c('train','test'),
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))
nn_dat %>% count(partition)


x_train = nn_dat %>%
  filter(partition == 'train') %>%
  select(8:ncol(nn_dat)) %>%
  as.matrix
y_train = nn_dat %>%
  filter(partition == 'train') %>%
  pull(class_num) %>%
  to_categorical(max(nn_dat$class_num) + 1)

x_test = nn_dat %>%
  filter(partition == 'test') %>%
  select(8:ncol(nn_dat)) %>%
  as.matrix
y_test = nn_dat %>%
  filter(partition == 'test') %>%
  pull(class_num) %>%
  to_categorical(max(nn_dat$class_num) + 1)


model = keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'relu', input_shape = 4) %>% 
  layer_dense(units = 3, activation = 'softmax')


model %>%
  compile(loss = 'categorical_crossentropy',
          optimizer = optimizer_rmsprop(lr = 0.01),
          metrics = c('accuracy')
  )


history = model %>%
  fit(x = x_train,
      y = y_train,
      epochs = 200,
      batch_size = 20,
      validation_split = 0.2
  )


plot(history)

perf = model %>% evaluate(x_test, y_test)
perf

plot_dat = nn_dat %>%
  filter(partition == 'test') %>%
  mutate(class_num = factor(class_num),
         y_pred = factor(predict_classes(model, x_test)),
         Correct = factor(ifelse(class_num == y_pred, "Yes", "No")))
plot_dat %>% select(-contains("feat")) %>% head(3)


title     = "Classification Performance of Artificial Neural Network"
sub_title = str_c("Accuracy = ", round(perf$acc, 3) * 100, "%")
x_lab     = "True iris class"
y_lab     = "Predicted iris class"
accuracy_plot <- plot_dat %>% ggplot(aes(x = class_num, y = y_pred, colour = Correct)) +
  geom_jitter() +
  scale_x_discrete(labels = levels(nn_dat$class_label)) +
  scale_y_discrete(labels = levels(nn_dat$class_label)) +
  theme_bw() +
  labs(title = title, subtitle = sub_title, x = x_lab, y = y_lab)

