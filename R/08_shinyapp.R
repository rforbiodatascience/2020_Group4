# Dependencies
list.of.packages <- c("shiny", "plotly", "maps", "tidyverse", "rsconnect")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if( length(new.packages) ) {
  install.packages(new.packages)
}

library(tidyverse)
library(plotly)
library(shiny)
library(maps)
library(rsconnect)

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
  snakes <- str_c(c(input$snake1, input$snake2), collapse = "|")
  
  # Detect the choices
  if( snakes == "|" ){
    snakes <- NULL
  }else if( endsWith(snakes, "|") ){
    snakes <- input$snake1
  }else if( startsWith(snakes, "|") ){
    snakes <- input$snake2
  }
  
  # If no choices, show all data
  if( !is.null(snakes) ){
    data <- data %>% 
      filter(str_detect(str_to_lower(Snake), str_to_lower(snakes)))
  }else{data}
}

server <- function(input, output) {
  dataset <- reactive({
    # Detect whether the venoms should be merged
    if( input$merge){
      data_aug %>%
        apply_filter(input) %>% 
        pivot_longer(all_of(toxin_names),
                     names_to = "Toxin",
                     values_to = "Value") %>% 
        group_by(Snake, Toxin) %>%
        summarise(mean(Value)) %>%
        mutate(Value = round(`mean(Value)`, 2))
    }else{
      data_aug %>% 
        apply_filter(input) %>% 
        mutate(Snake = paste(Snake, " (",
                             row_number(), ")",
                             sep = "")) %>%
        pivot_longer(all_of(toxin_names),
                     names_to = "Toxin",
                     values_to = "Value")
    }
  })
  
  # Generate plot
  output$plot <- renderPlotly({
    plot <- dataset() %>% 
      ggplot(aes(y = Snake, x = Value, fill = Toxin)) +
      geom_col() +
      labs(x = "Venom Composition (%)",
           y = "Snake",
           title = "Comparing venom composition") +
      theme(legend.position = "none")
  })
}
shinyApp(ui, server)
# deployApp(appName = "compareTwo")
