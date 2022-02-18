summaryUI <- function(){
	
	tagList(
		titlePanel("Summary"),
		
		column(
			width = 2,
			tableOutput("summary_table")
		),
		
		column(
			width = 10,
			plotOutput("summary_plot")
		)
	)
	
}