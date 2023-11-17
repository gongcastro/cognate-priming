#' Run the targets pipeline as a job in RStudio
#' 
#' @details
#' Running the targets workflow may take some time. To free the R console while targets runs, this functions calls [job::job] to move the execution to a different R session.
#' 
make <- function(target) {
	job::job(targets =  {
		targets::tar_make()
		job::export("none")  # return nothing
	},  
	import = NULL,
	title = "Cognate Priming",
	packages = NULL)
}

#' Destroy targets products
#' 
unmake <- function(keep_rds = TRUE) {
	tar_destroy(ask = FALSE)
	rds_paths <- list.files("results/", pattern = "\\.rds", full.names = TRUE)
	if (!keep_rds){
		if (length(rds_paths) > 0) lapply(rds_paths, file.remove)
	}
	cli_alert_success("Removed project outputs!")
}

#' Print welcome message in console
#' 
welcome_message <- function() {
	id <- cli_status("")
	cli({
		cli_h1("cognate-beginnings")
		cli_text("This project works with the {.pkg renv} and 
                     {.pkg targets} R packages.")
		cli_text()
		cli_li("{.pkg renv} manages package dependencies so that you can
                   install the packages needed to run the code in this repository
                   in their correct versions, without touching the packages that
                   you already have installed in your machine. This ensures that
                   the code and R packages that you use in your other projects is
                   not affected by running the code in this project.")
		cli_text()
		cli_li("{.pkg targets} manages reproducible workflows. The file 
                   {.path _targets.R} contains the commands to be run in order to get the 
                   outcomes and byproducts of the code in this repository. These
                   functions are defined in the scripts in {.path R/}. See the
                   instructions below to run the workflow.")
		
		cli_h2("Instructions")
		cli_ol(c("Run {.code renv::restore()}", 
				 "Restart the R session",  
				 "Run {.code tar_make()}"))
		cli_h2("Repository info")
		cli_ul(c("Lab notes: {.url gongcastro.github.io/cognate-beginnings}",
				 "URL: {.url https://github.com/gongcastro/cognate-beginnings}",
				 "OSF: {.url https://osf.io/hy984/}"
		))
	})
	cli_status_clear(id)
}

#' Delete unwanted files
#' 
clean_repo <- function() {
	
	file.paths <- file.path(c("manuscript/orcidlink.sty",
							  "manuscript/arxiv.sty",
							  ".luarc.json",
							  "_targets.yaml"))
	
	file.paths <- file.paths[file.exists(file.paths)]
	if(length(file.paths) > 0) {
		invisible(lapply(file.paths, file.remove))
		cli::cli_progress_step("Deleting {.path {file.paths}} file{?s}")
	}
	
	# delete directories
	dir.paths <- c("manuscript/manuscript_files/",
				   "manuscript/appendix_files/")
	dir.paths <- dir.paths[dir.exists(dir.paths)]
	if (length(dir.paths) > 0) {
		invisible(lapply(dir.paths, unlink, recursive = TRUE))
		cli::cli_progress_step("Deleting {.path {dir.paths}} director{?y/ies}")
	}
}
