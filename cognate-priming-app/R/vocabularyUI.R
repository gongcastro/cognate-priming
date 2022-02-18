vocabularyUI <- function(){
	
	tagList(
		# title
		titlePanel("Vocabulary"),
		
		# tabs
		tabsetPanel(
			tabPanel("Summary"),
			tabPanel("Total"),
			tabPanel("L1"),
			tabPanel("L2"),
			tabPanel("Conceptual"),
			tabPanel("Translation equivalents")
		)	
	)
	
}