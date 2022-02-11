# source("renv/activate.R")

if ("targets" %in% utils::installed.packages()) library(targets)

source("src/utils.R")

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


# load all built targets (and packages)
tar_load_all <- function(){
	invisible({
		suppressMessages({
			tar_load_globals()
		})
		tars <- tar_objects()
		message("Loading targets: ", paste0(tars, collapse = ", "))
		lapply(tars, tar_load_raw, envir = .GlobalEnv)
	})
}

unmake <- function() {
	path <- "results/fit.rds"
	tar_destroy(ask = FALSE)
	if (file.exists(path)) {
		file.remove(path)
	}
	"Removed project outputs!"
}

message('Welcome! To execute the repository, 1) run renv::restore(), 2) and then make() in your R console.')
message('Check https://github.com/bilingual-project/cognate-priming for instructions, documentation, and troubleshooting.')

options(knitr.duplicate.label = "allow")
