# Add new data

# install.packages("googlesheets4")

library(googlesheets4)

new_data <- read_sheet('https://docs.google.com/spreadsheets/d/1vLrvvQmQdvCtr6n0hjDbIoLiOv3cHVGx7MK_2w_WH3E/edit#gid=0',
                       sheet = 1,
                       col_types = 'cnnnn')
new_meta <- read_sheet('https://docs.google.com/spreadsheets/d/1vLrvvQmQdvCtr6n0hjDbIoLiOv3cHVGx7MK_2w_WH3E/edit#gid=0',
                       sheet = 2)



new_data %>% 
  replace_na(list(everything() = 0)) %>% 
  View()
new_data[is.na(new_data)] <- 0
