library(targets)
library(tarchetypes)

# load functions
source("R/utils.R")
source("R/00_stimuli.R")
source("R/01_participants.R")
source("R/02_vocabulary.R")
source("R/03_gaze_bcn.R")
source("R/03_gaze_oxf.R")
source("R/04_attrition.R")
source("R/05_prepare.R")
source("R/06_analysis.R")

# load packages ----
tar_option_set(
	packages = c(
		"dplyr", "tidyr", "stringr", "multilex", "keyring",
		"readxl", "janitor", "childesr", "mice", "here",
		"googlesheets4", "lubridate", "httr", "data.table",
		"purrr", "eyetrackingR", "brms", "tidybayes", "shiny",
		"rmarkdown", "knitr", "patchwork", "scales"
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
	tar_target(trials_path, here("Stimuli", "stimuli.xlsx"), format = "file"), # trial list,
	tar_target(stimuli_english_path, here("Data", "Stimuli", "stimuli_english.xlsx"), format = "file"), # stimuli info for English version
	tar_target(animacy_path, here("Data", "Stimuli", "animacy.csv"), format = "file"), # animacy of stimuli (hand coded)
	
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
	tar_target(
		participants_oxf_path,
		here("Data", "Participants", "participant_oxford_Apr2021.xlsx"),
		format = "file"
	),
	# import Oxford data
	tar_target(
		participants_oxf,
		read_xlsx(
			participants_oxf_path,
			na = "NA"
		)
	),
	# import Barcelona data from Google Sheets
	# in the future, this will be a local CSV
	tar_target(
		participants_bcn,
		range_read(
			ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE",
			sheet = "barcelona", na = ""
		)
	),
	# join datasets
	# see R/01_participants.R for details on this function
	tar_target(
		participants,
		get_participants(participants_bcn, participants_oxf)
	),
	
	# vocabulary ----
	# define Oxford file path
	tar_target(
		vocabulary_oxf_path,
		here("Data", "Vocabulary", "vocabulary_oxford_Apr2021.xlsx")
	),
	# import Oxford data
	tar_target(
		vocabulary_oxf,
		vocabulary_oxf_path %>% 
			# get sheets names
			excel_sheets() %>%
			# read all sheets into a list
			map(
				~read_xlsx(
					vocabulary_oxf_path,
					sheet = .,
					na = c("", "NA", "?", "x")
				)
			) %>%
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
			here("Data", "Gaze", "Barcelona"),
			full.names = TRUE
		)
	),
	
	# import data
	# see R/03_gaze_bcn.R and utils.R for details on this function
	tar_target(
		gaze_bcn,
		get_gaze_bcn(
			file_paths = gaze_bcn_paths,
			participants = participants,
			stimuli = stimuli
		)
	),
	
	# gaze data (Oxford) ----
	# define file paths
	tar_target(
		gaze_oxf_paths,
		list.files(
			here("Data", "Gaze", "Oxford"),
			full.names = TRUE
		)
	),
	# import data
	# see R/03_gaze_oxf.R for details on this function
	tar_target(
		gaze_oxf,
		get_gaze_oxf(
			file_paths = gaze_oxf_paths,
			participants = participants,
			stimuli = stimuli
		)
	),
	
	# attrition data ----
	# see R/04_attrition.R for details on this function
	# stringent version
	tar_target(
		attrition,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			# minimum looking time to each picture for valid trial
			looking_threshold = c(prime = 250, target = 250, distractor = 0), 
			# minimum number of valid trials in each condition for valid participant
			missing_trials_threshold = c(cognate = 2, noncognate = 2, unrelated = 2),
			# what words should the participant know for valid trial?
			filter_vocabulary = c("prime", "target"),
			# should target-distractor counterbalancing pairs we filtered out together?
			# e.g. if TRUE, cat-balloon removed (even if valid) if balloon-cat is not valid
			filter_counterbalancing = FALSE
		)
	),
	# relaxed version
	# same function, different argument values (less stringent version)
	tar_target(
		attrition_relaxed,
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
	# stringent version with filter_counterbalancing = TRUE
	tar_target(
		attrition_counter,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			looking_threshold = c(prime = 250, target = 250, distractor = 0), # minimum looking time
			missing_trials_threshold = c(cognate = 2, noncognate = 2, unrelated = 2), # minimum n trials in each condition
			filter_counterbalancing = TRUE,
			filter_vocabulary = c("prime", "target")
		)
	),
	# relaxed version with filter_counterbalancing = TRUE
	tar_target(
		attrition_relaxed_counter,
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
	# relaxed version
	tar_target(
		gaze_relaxed,
		prepare_data(
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			participants = participants,
			stimuli = stimuli, 
			vocabulary = vocabulary,
			attrition = attrition_relaxed
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
					(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp + age_group +
					(1 + time_bin_center*trial_type + age_group | participant),
				family = gaussian
			)
		)
	),
	# define the dataset corresponding to each model (same order as in previous target)
	# stringent inclusion criteria
	tar_target(
		model_datasets,
		list(
			fit = gaze
		)
	),
	# same datasets, but applying the relaxed inclusion criteria
	tar_target(
		model_datasets_relaxed,
		list(
			fit_relaxed = gaze_relaxed
		)
	),
	# fit models (stringent criteria)
	tar_target(
		model_fits,
		fit_models(
			formulas = model_formulas,
			datasets = model_datasets, 
			file = here("Results", "fit.rds"),
			save_model = here("Stan", "fit.stan")
		)
	),
	# fit model (relaxed criteria)
	tar_target(
		model_fits_relaxed,
		fit_models(
			formulas = model_formulas,
			datasets = model_datasets_relaxed,
			file = here("Results", "fit_relaxed.rds"),
			save_model = here("Stan", "fit_relaxed.stan")
		)
	),
	# render report.Rmd with the updated model fits
	tar_render(report, "Rmd/report.Rmd")
	
)


