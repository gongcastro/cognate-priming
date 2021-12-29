source("renv/activate.R")

if ("targets" %in% installed.packages()) library(targets)

library(targets)

make <- function() {
	job::job(
		{{ 
			targets::tar_make()
			job::export("none")  # return nothing
		}}, 
		import = NULL,
		title = "Cognate Priming"
		
	)
}

unmake <- function() {
	path <- "Results/fit.rds"
	tar_destroy(ask = FALSE)
	if (file.exists(path)) {
		file.remove(path)
	}
	"Removed project outputs!"
}

message('Welcome! To execute the repository, 1) run renv::restore(), 2) and then make() in your R console.')
message('Check https://github.com/bilingual-project/cognate-priming for instructions, documentation, and troubleshooting.')

options(knitr.duplicate.label = "allow")
