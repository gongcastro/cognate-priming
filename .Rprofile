source("renv/activate.R")
library(targets)

make <- function() {
	job::job({{ targets::tar_make() }}, title = "Cognate Priming")
}

options(knitr.duplicate.label = "allow")
