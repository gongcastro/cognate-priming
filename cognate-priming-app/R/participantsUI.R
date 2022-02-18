participantsUI <- function(){
	
	tagList(
		# title
		titlePanel("Participants"),
		
		# tabs
		tabsetPanel(
			tabPanel("Summary"),
			tabPanel("Language profiles")
		)		
	)
	
}