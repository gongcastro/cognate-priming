modelUI <- function(){
	
	tagList(
		# title
		titlePanel("Model outputs"),
		
		# tabs
		tabsetPanel(
			tabPanel("Population-level effects"),
			tabPanel("Group-level effects"),
			tabPanel("Population-level predictions"),
			tabPanel("Group-level predictions"),
			tabPanel("Model diagnostics")
		)
	)
	
}