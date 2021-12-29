library(targets)
library(tarchetypes)

# load functions
source("src/R/utils.R")
source("src/R/00_stimuli.R")
source("src/R/01_participants.R")
source("src/R/02_vocabulary.R")
source("src/R/03_gaze_bcn.R")
source("src/R/03_gaze_oxf.R")
source("src/R/04_attrition.R")
source("src/R/05_prepare.R")
source("src/R/06_analysis.R")

# load packages ----
tar_option_set(
	packages = c(
		# project utils
		"targets", "tarchetypes", "here",
		# data manipulation
		"dplyr", "tidyr", "purrr", "stringr", "tibble", "forcats", 
		"lubridate", "data.table", "janitor",
		# data retrieval
		"readxl", "janitor", "childesr", "multilex", "keyring", "googlesheets4", "httr",
		# modelling
		"eyetrackingR", "brms", "tidybayes", "emmeans", "mice", 
		# data visualisation
		"ggplot2", "shiny", "patchwork", "shiny",
		# data reporting
		"rmarkdown", "knitr", "papaja", "scales", "DiagrammeR", "gt",
		# unit testing
		"testthat"
	)
)


# define targets (see https://books.ropensci.org/targets/)
# in each target, the value returned by a function (second argument) is assigned to 
# a variable name (first argument). This object is stored in in the _targets folder
# and can be imported into the Global environment using tar_load()
# each target takes some input. Some targets get the result of other targets as inputs
# you can visualise the dependencies between files, scripts/functions and targets by running tar_visnetwork()

# to execute this script and generate the targets, run tar_make() in your console. This will
# (1) Skip those targets that are already up to date (nothing they depend on has changed since last time you run tar_make())
# (2) Run outdated targets (those for which something they depend on has changed since last time)
# the first time you run, tar_make() may take a while to run entirely
# it will take shorter after the first time, as it only runs the outdated targets

list(
	
	# get multilex data (Barcelona vocabulary data) ----
	# log into multilex
	tar_target(credentials, get_credentials()),
	# this returns a list with all necessary data
	tar_target(multilex_data, get_multilex(update = TRUE, type = "understands")),
	
	# stimuli ----
	# define file paths
	tar_target(trials_path, here("stimuli", "stimuli.xlsx"), format = "file"), # trial list,
	tar_target(stimuli_english_path, here("data", "stimuli", "stimuli_english.xlsx"), format = "file"), # stimuli info for English version
	tar_target(animacy_path, here("data", "stimuli", "animacy.csv"), format = "file"), # animacy of stimuli (hand coded)
	
	# import data
	tar_target(trials, read_xlsx(trials_path)),
	tar_target(stimuli_english, read_xlsx(stimuli_english_path, na = "NA")),
	tar_target(animacy, read.csv(animacy_path)),
	
	# join all stimuli datasets into a single object
	# see R/00_stimuli.R for details on this function
	tar_target(
		stimuli,
		get_stimuli(
			trials = trials,
			animacy_data = animacy,
			oxford_data = stimuli_english,
			multilex_data = multilex_data
		)
	),
	
	# participants ----
	# define Oxford file paths
	tar_target(participants_oxf_path, here("data", "participants", "participant_oxford_Apr2021.xlsx"), format = "file"),
	# import Oxford data
	tar_target(participants_oxf, read_xlsx(participants_oxf_path, na = "NA")),
	# import Barcelona data from Google Sheets
	# in the future, this will be a local CSV
	tar_target(participants_bcn, range_read(ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", sheet = "barcelona", na = "")),
	# join datasets
	# see R/01_participants.R for details on this function
	tar_target(participants, get_participants(participants_bcn, participants_oxf)),
	# vocabulary ----
	# define Oxford file path
	tar_target(vocabulary_oxf_path, here("data", "vocabulary", "vocabulary_oxford_Apr2021.xlsx")),
	# import Oxford data
	tar_target(
		vocabulary_oxf,
		vocabulary_oxf_path %>% 
			# get sheets names
			excel_sheets() %>%
			# read all sheets into a list
			map(~read_xlsx(vocabulary_oxf_path, sheet = ., na = c("", "NA", "?", "x"))) %>%
			# name each sheet
			set_names(excel_sheets(vocabulary_oxf_path)) %>%
			# join sheets into a data frame
			#new variable (version) identifies each sheet
			bind_rows(.id = "version")
	),
	# join all vocabulary datasets
	tar_target(
		vocabulary,
		# see R/02_vocabulary.R for details on this function
		get_vocabulary(
			participants = participants, 
			update = FALSE,
			type = "understands",
			impute = TRUE,
			vocabulary_oxf = vocabulary_oxf,
			multilex_data = multilex_data
		)
	),
	
	# gaze data (Barcelona) ----
	# define file paths
	tar_target(
		gaze_bcn_paths,
		list.files(
			here("data", "gaze", "barcelona"),
			full.names = TRUE
		)
	),
	
	# import data
	# see R/03_gaze_bcn.R and utils.R for details on this function
	tar_target(gaze_bcn, get_gaze_bcn(file_paths = gaze_bcn_paths, participants = participants, stimuli = stimuli)),
	
	# gaze data (Oxford) ----
	# define file paths
	tar_target(gaze_oxf_paths, list.files(here("data", "gaze", "oxford"), full.names = TRUE)),
	# import data
	# see R/03_gaze_oxf.R for details on this function
	tar_target(gaze_oxf, get_gaze_oxf(file_paths = gaze_oxf_paths, participants = participants, stimuli = stimuli)),
	
	# attrition data ----
	# see R/04_attrition.R for details on this function
	tar_target(
		attrition,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			looking_threshold = c(prime = 250, target = 250, distractor = 0),
			missing_trials_threshold = c(cognate = 0, noncognate = 0, unrelated = 0),
			filter_vocabulary = NULL,
			filter_counterbalancing = FALSE
		)
	),
	tar_target(
		attrition_counter,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			looking_threshold = c(prime = 250, target = 250, distractor = 0), # minimum looking time
			missing_trials_threshold = c(cognate = 0, noncognate = 0, unrelated = 0), # minimum n trials in each condition
			filter_counterbalancing = TRUE,
			filter_vocabulary = NULL
		)
	),
	# prepare data for analysis ----
	# see R/05_prepare.R for details on the get_prepared() function
	# this function returns an analysis ready dataset, with all necessary (transformed/coded) variables
	# data aggregation and time window definition is performed using the eyetrackingR package
	# stringent version
	tar_target(
		gaze,
		prepare_data(
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			participants = participants,
			stimuli = stimuli, 
			vocabulary = vocabulary,
			attrition = attrition
		)
	),
	
	# fit models ----
	# see R/06_analysis.R for details on the fit_models() function
	# this function takes a list of formulas and list of datasets and fits a model 
	# that takes each formula-dataset pair at a time, and returns a named list of fits
	tar_target(
		model_formulas,
		list(
			# this model includes all data and all predictors of interest
			fit = bf(
				formula = logit_adjusted ~
					(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp*age_group +
					(1 + time_bin_center*trial_type*age_group | participant) +
					(1 + time_bin_center*trial_type*lp*age_group | target),
				family = gaussian
			)
		)
	),
	
	# define the dataset corresponding to each model (same order as in previous target)
	tar_target(
		model_datasets,
		list(
			fit = gaze
		)
	),
	tar_target(
		model_fits,
		fit_models(
			formulas = model_formulas,
			datasets = model_datasets, 
			file = here("results", "fit.rds"),
			save_model = here("src", "stan", "fit.stan")
		)
	),

	# render docs
	tar_render(docs_participants, "docs/00_participants.Rmd"),
	tar_render(docs_stimuli, "docs/01_stimuli.Rmd"),
	tar_render(docs_vocabulary, "docs/02_vocabulary.Rmd"),
	tar_render(report, "docs/03_design.Rmd"),
	tar_render(docs_analysis, "docs/04_analysis.Rmd"),
	tar_render(docs_attrition, "docs/05_attrition.Rmd"),
	tar_render(docs_results, "docs/06_results.Rmd"),
	
	# render presentations
	tar_render(communications_lacre, "presentations/2022-01-25_lacre/2022-01-25_lacre-abstract.Rmd"),
	tar_render(communications_icis, "presentations/2022-07-07_icis/2022-07-07_icis-abstract.Rmd")
	
	# render manuscript
)


