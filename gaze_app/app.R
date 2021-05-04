#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# import data ----
gaze_time <- readRDS("Results/gaze_time.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cognate Priming gaze data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "participant",
                label = "Participant",
                choices = unique(gaze_time$participant)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("gaze_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$gaze_plot <- renderPlot({
        gaze_time %>% 
            filter(participant %in% input$participant) %>% 
            ggplot(aes(time_bin))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
