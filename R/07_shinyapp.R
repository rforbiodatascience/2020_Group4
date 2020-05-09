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

data_aug <- read_csv("03_data_aug.csv")
toxins <- data_aug %>% 
  select_if(is.numeric)
ui <- fluidPage(
  fluidRow(
    column(3,
           wellPanel(
             textInput("snake1", "Snake:", value = "Bothrops atrox"),
             textInput("snake2", "Another snake:", value = "Naja kaouthia")
           )
    ),
    column(9,
           plotlyOutput("plot", height = "400px"))
  )
)

apply_filter <- function(data, input){
  snakes <- str_c(c(input$snake1, input$snake2), collapse = "|")
  
  if( snakes == "|" ){
    snakes <- NULL
  }else if( endsWith(snakes, "|") ){
    snakes <- input$snake1
  }else if( startsWith(snakes, "|") ){
    snakes <- input$snake2
  }
  
  if( !is.null(snakes) ){
    data <- data %>% 
      filter(str_detect(str_to_lower(Snake), str_to_lower(snakes)))
  }else{data}
}

server <- function(input, output) {
  dataset <- reactive({
    data_aug %>%
      apply_filter(input) %>% 
      pivot_longer(colnames(toxins),
                   names_to = "Toxin",
                   values_to = "Value") %>% 
      group_by(Snake, Toxin) %>%
      summarise(mean(Value)) %>%
      mutate(Value = round(`mean(Value)`, 2)) %>%
      filter(Value > 0)
  })
  
  output$plot <- renderPlotly({
    plot <- dataset() %>% 
      ggplot(aes(y = Snake, x = Value, fill = Toxin)) +
      geom_col() +
      labs(x = "Mean Venom Composition (%)",
           y = "Snake",
           title = "Comparing venom composition") +
      theme(legend.position = "none")
  })
}
shinyApp(ui = ui, server = server)
# deployApp(appName = "compareTwo")
