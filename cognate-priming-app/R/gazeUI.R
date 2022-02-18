gazeUI <- function(){
	
	tagList(
		# title
		titlePanel("Gaze data"),
		
		# tabs
		tabsetPanel(
			tabPanel("Summary"),
			tabPanel("Attrition")
		)
	)
	
}