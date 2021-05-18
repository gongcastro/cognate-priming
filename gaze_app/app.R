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
gaze <- readRDS("../Results/gaze_raw.rds") %>% 
    mutate(fix_any = case_when(
        fix_target ~ "Target",
        fix_distractor ~ "Distractor",
        TRUE ~ "Other"
    ))

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
            ),
            selectInput(
                inputId = "age_group",
                label = "Age",
                choices = unique(gaze_time$age_group)
            ),
            selectInput(
                inputId = "trial_num",
                label = "Trial",
                choices = unique(gaze_time$trial_num)
            )
        ),
        
        fluidRow(
            mainPanel(
                plotOutput("gaze_plot"),
                plotOutput("gaze_time")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$gaze_plot <- renderPlot({
        gaze %>% 
            filter(
                participant %in% input$participant,
                age_group %in% input$age_group,
                trial_num %in% input$trial_num
            ) %>% 
            ggplot(aes(x, y, colour = time)) +
            geom_point(aes(group = trial_num)) +
            annotate(geom = "rect", xmin = 230, xmax = 730, ymin = 290, ymax = 790,
                     fill = NA, colour = "black") +
            annotate(geom = "rect", xmin = 1190, xmax = 1690, ymin = 290, ymax = 790,
                     fill = NA, colour = "black") +
            labs(x = "X-axis", y = "Y-axis", colour = "Time (ms)") +
            scale_x_continuous(limits = c(0, 1920)) +
            scale_y_continuous(limits = c(0, 1080)) +
            coord_fixed() +
            theme_bilingual() +
            theme(
                panel.border = element_rect(fill = NA, colour = "black", size = 1),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank()
            )
    })
    
    output$gaze_time <- renderPlot({
        gaze %>% 
            filter(
                participant %in% input$participant,
                age_group %in% input$age_group,
                trial_num %in% input$trial_num
            ) %>% 
            pivot_longer(c(x, y), names_to = "coord", values_to = "value") %>% 
            ggplot(aes(time, value, colour = is_imputed)) +
            facet_wrap(~coord, scales = "free", nrow = 2) +
            geom_point(size = 2) +
            labs(x = "Time (ms)", y = "Value", colour = "Is imputed") +
            scale_color_brewer(palette = "Set1") +
            scale_y_continuous(limits = c(0, 1920)) +
            theme_bilingual() +
            theme(
                panel.grid = element_line(linetype = "dotted", colour = "grey"),
                legend.position = "top"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
