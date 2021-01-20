library(shinyWidgets)
library(rvest)
library(tidytext)
library(tidyverse)
library(lsa)
library(tm)


source("load_data.R", local = TRUE)$value

ui <- fluidPage(
  sidebarPanel(
    h2("Introduction"),
    h5("This shiny app is a simple content-based recommender for holiday destinations."),
    uiOutput("tab"),
    h5("To use this recommender, select from the dropdown menue 1 place you have been to or you are interested in visiting. When you press the run button, 5
       holiday destinations based on your choice will be displayed."),
    br(),
    h2("Select a place"),
    pickerInput(inputId = "place_selection",
                label = "",
                choices = holiday_des$destinations,
                 options = pickerOptions(
                  actionsBox = FALSE,
                  maxOptions = 1 # maximum of options
                ) 
              ),
    h4(" "),
    textOutput("place01"),
    actionButton("run", "Run")
    
  ),
  mainPanel(
    img(src="queenstown.jpg", width="90%"),
    tableOutput("recomm")
  )
)


server <- function(input, output, session) {
 
  source("data_server.R", local = TRUE)$value
  
  }


shinyApp(ui = ui, server = server)