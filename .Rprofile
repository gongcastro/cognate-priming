source("renv/activate.R")


options(crayon.enabled = TRUE,
		repos = c(CRAN = "https://cloud.r-project.org",
				  gongcastro = "https://gongcastro.r-universe.dev",
				  stan = "https://mc-stan.org/r-packages/"))

if (interactive()) {
	
	# load packages
	suppressWarnings({
		library(targets)
	})
	
	source(file.path("src", "helpers.R"))
	
	resolve_conflicts()
	
	cli::cli_alert_info('Welcome! To execute the repository, 1) run `renv::restore()`, 2) and then `make()` in your R console.')
	cli::cli_alert_info('Check {.url https://github.com/gongcastro/cognate-priming} for instructions, documentation, and troubleshooting.')
	
}

