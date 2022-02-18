summaryServerTable <- function(id, participants) {
	
	moduleServer(
		id,
		
		function(input, output, session) {
			
			participants <- reactive({
				readRDS(here("data", "participants.rds"))
			})
			
			counts <- reactive({
				participants() %>% 
					count(location, lp, age_group, test_language) %>% 
					pivot_wider(names_from = age_group, values_from = n)	
			})
		
			output$summary_table <- renderTable({
				counts()
			})
			
			return(participants)
		})
	
}
