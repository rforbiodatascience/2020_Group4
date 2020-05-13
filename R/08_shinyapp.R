rm(list = ls())

# Dependencies
list.of.packages <- c("shiny", "plotly", "tidyverse") # "rsconnect"

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if( length(new.packages) ) {
  install.packages(new.packages)
}

library(tidyverse)
library(plotly)
library(shiny)
# library(rsconnect)

# Load data
data_aug <- read_csv("03_data_aug.csv")
# Vector containing all toxin names
toxin_names <- data_aug %>% 
  select_if(is.numeric) %>% 
  colnames()

# The app UI
ui <- fluidPage(
  fluidRow(
    column(3,
           wellPanel(
             textInput("snake1", "Snake 1:", value = "Bungarus candidus"),
             textInput("snake2", "Snake 2:", value = "Daboia russelii russelii"),
             checkboxInput("merge", "Merge species", value = TRUE)
           )
    ),
    column(9,
           plotlyOutput("plot", height = "400px"))
  )
)

# Filter based on the given input
# No input returns an empty string
apply_filter <- function(data, input){
  input_snakes <- str_c(c(input$snake1, input$snake2), collapse = "|")
  
  # Detect the choices
  if( input_snakes == "|" ){
    input_snakes <- NULL
  }else if( endsWith(x = input_snakes, suffix =  "|") ){
    input_snakes <- input$snake1
  }else if( startsWith(x = input_snakes, prefix = "|") ){
    input_snakes <- input$snake2
  }
  
  # If no choices, show all data
  if( !is.null(input_snakes) ){
    data <- data %>% 
      filter(str_detect(str_to_lower(Snake), str_to_lower(input_snakes)))
  }else{data}
}

server <- function(input, output) {
  dataset <- reactive({
    # Detect whether the venoms should be merged
    if( input$merge ){
      data_aug %>%
        apply_filter(input) %>% 
        pivot_longer(cols = all_of(toxin_names),
                     names_to = "Toxin",
                     values_to = "toxin_amount") %>% 
        group_by(Snake, Toxin) %>%
        summarise(`Toxin amount (%)` = toxin_amount %>% 
                    mean() %>%
                    round(2))
    }else{
      data_aug %>% 
        apply_filter(input) %>% 
        mutate(Snake = str_c(Snake, " (",
                             row_number(), ")")) %>%
        pivot_longer(cols = all_of(toxin_names),
                     names_to = "Toxin",
                     values_to = "Toxin amount (%)")
    }
  })
  
  # Generate plot
  output$plot <- renderPlotly({
    plot <- dataset() %>% 
      ggplot(aes(y = Snake, x = `Toxin amount (%)`, fill = Toxin)) +
      geom_col() +
      labs(x = "Venom Composition (%)",
           y = "Snake",
           title = "Comparing venom composition") +
      theme(legend.position = "none")
  })
}
shinyApp(ui, server)
# deployApp(appName = "compareTwo")
