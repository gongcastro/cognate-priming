# raw data app

raw_data_app <- function(data){
    
    clean <- data %>% 
        mutate(participant_age = paste0(participant, " (", age_group, ")"))
    
    ui <- fluidPage(
        
        titlePanel("Explore gaze data"),
        
        column(
            width = 4,
            selectInput(
                "participant_age",
                "Participant:",
                choices = sort(unique(clean$participant_age))
            ),
            selectInput(
                "trial",
                "Trial:",
                choices = 1:32
            ),
            htmlOutput(outputId = "text")
        ),
        
        column(
            width = 8,
            plotOutput("plot")
        )
    )
    
    server <- function(input, output) {
        
        output$plot <- renderPlot({
            clean %>% 
                filter(
                    participant_age %in% input$participant_age,
                    trial %in% input$trial
                ) %>% 
                mutate(aoi = case_when(aoi_target ~ "Target", aoi_distractor ~ "Distractor", TRUE ~ "Other")) %>% 
                pivot_longer(c(x, y), names_to = "coord", values_to = "value") %>%
                ggplot(aes(time_stamp, value, colour = aoi)) +
                facet_wrap(~coord, ncol = 1, strip.position = "left") +
                geom_line(aes(group = 1), size = 1) +
                labs(x = "Time (s)", y = "Gaze position", colour = "Fixated AOI") +
                theme(
                    legend.position = "top",
                    panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
                )
            
        })
        
        output$text <- renderUI({
            lp <- filter(
                clean, participant_age %in% input$participant_age, 
                trial %in% input$trial
            ) %>% 
                pull(lp) %>%
                unique() %>%
                paste0("<b>LP</b>: ", .)
            location <- filter(
                clean, participant_age %in% input$participant_age, 
                trial %in% input$trial
            ) %>% 
                pull(location) %>%
                unique()%>%
                paste0("<b>Location</b>: ", .)
            list <- filter(
                clean, participant_age %in% input$participant_age, 
                trial %in% input$trial
            ) %>% 
                pull(list) %>%
                unique()%>%
                paste0("<b>List</b>: ", .)
            trial_type <- filter(
                clean, participant_age %in% input$participant_age, 
                trial %in% input$trial
            ) %>% 
                pull(trial_type) %>%
                unique() %>%
                paste0("<b>Trial type</b>: ", .)
            prime <- filter(
                clean, participant_age %in% input$participant_age, 
                trial %in% input$trial
            ) %>% 
                pull(prime) %>%
                unique() %>% 
                str_remove("cat_|spa_") %>% 
                paste0("<b>Prime</b>: ", .)
            target <- filter(
                clean, participant_age %in% input$participant_age, 
                trial %in% input$trial
            ) %>% 
                pull(target) %>%
                unique() %>%
                str_remove("cat_|spa_") %>% 
                paste0("<b>Target</b>: ", .)
            
            x <- HTML(paste(lp, location, test_language, list, trial_type, prime, target, sep = "<br/>"))
            
            return(x)
            
            
        })
    }
    
    shinyApp(ui = ui, server = server)
    
}
