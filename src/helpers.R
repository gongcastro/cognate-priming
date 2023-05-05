# project utils ----------------------------------------------------------------

# run the targets pipeline
make <- function() {
	job::job({{ 
		targets::tar_make()
		job::export("none")  # return nothing
	}}, 
	import = NULL,
	title = "Cognate Priming")
}

# destroy targets products
unmake <- function(keep_rds = TRUE) {
	tar_destroy(ask = FALSE)
	rds_paths <- list.files("results/", pattern = ".rds", full.names = TRUE)
	if (!keep_rds){
		if (length(rds_paths) > 0) lapply(rds_paths, file.remove)
	}
	cli::cli_alert_success("Removed project outputs!")
}

# resolve namespace conflicts between packages
resolve_conflicts <- function() {
	suppressMessages({
		conflicted::conflict_prefer("last_warnings", "rlang")
		conflicted::conflict_prefer("filter", "dplyr")
		conflicted::conflict_prefer("between", "dplyr")
		conflicted::conflict_prefer("timestamp", "utils")
		conflicted::conflict_prefer("ar", "brms")
		conflicted::conflict_prefer("chisq.test", "stats")
		conflicted::conflict_prefer("discard", "scales")
		conflicted::conflict_prefer("duration", "lubridate")
		conflicted::conflict_prefer("fisher.test", "stats")
		conflicted::conflict_prefer("lag", "dplyr")
	})
}
