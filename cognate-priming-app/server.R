
library(shiny)

theme_set(theme_minimal())

shinyServer(function(input, output) {

    participants <- reactive({
        readRDS(here("data", "participants.rds"))
    })
    
    counts <- reactive({
        participants() %>% 
            drop_na(location, lp, age_group) %>% 
            count(location, lp, age_group, .drop = FALSE) %>% 
            complete(location, age_group, lp, fill = list(n = as.integer(0))) %>% 
            pivot_wider(names_from = age_group, values_from = n)	
    })
    
    output$summary_table <- renderTable({
        counts()
    })
    
    output$summary_plot <- renderPlot({
        counts() %>% 
            ggplot() +
            aes(age_group, n, fill = lp) +
            facet_wrap(vars(location)) + 
            geom_col(position = position_dodge(), size = 1, colour = "white") +
            labs(
                x = "Age group",
                y = "Number of participants",
                fill = "Language profile",
                title = "Sample sizes"
            )
    })
    
    
})
