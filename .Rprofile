source("renv/activate.R")

library(targets)

make <- function() {
	job::job({{ targets::tar_make() }}, title = "Cognate Priming")
}

message('Welcome! To execute the repository, 1) run renv::restore(), 2) and then make() in your R console.')
message('Check https://github.com/bilingual-project/cognate-priming for instructions, documentation, and troubleshooting.')

options(knitr.duplicate.label = "allow")
