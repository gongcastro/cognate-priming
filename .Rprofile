source("renv/activate.R")

options(crayon.enabled = TRUE,
		repos = c(CRAN = "https://cloud.r-project.org",
				  gongcastro = "https://gongcastro.r-universe.dev",
				  "stan-dev" = "https://mc-stan.org/r-packages/"))

if (interactive()) {
	
	suppressWarnings({
		inst_pkgs <- utils::installed.packages()
		if (!all(c("targets", "cli") %in% inst_pkgs)) {
			message("Packages cli and targets must be installed.\nPlease, install them and restart your R session or run `renv::restore()`")
		} else {
			library(cli)
			library(targets)
		}
	})
	
	invisible({
		src_files <- list.files("src", pattern = ".R$", full.names = TRUE)
		lapply(src_files, source)
	})
	
	welcome_message()
}
