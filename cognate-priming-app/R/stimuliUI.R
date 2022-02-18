stimuliUI <- function(){
	
	tagList(
		# title
		titlePanel("Stimuli"),
		
		# tabs
		tabsetPanel(
			tabPanel("Summary"),
			tabPanel("Word forms"),
			tabPanel("Lexical frequency"),
			tabPanel("Word prevalence"),
			tabPanel("Semantic category")
		)
	)
	
}