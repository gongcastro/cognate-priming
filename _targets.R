# load packages ----------------------------------------------------------------

suppressPackageStartupMessages({
	suppressWarnings({
		# workflows and project
		library(targets)
		library(tarchetypes)
		library(cli)
		# data handling, cleaning, and testing
		library(tidyverse)
		library(testthat)
		# modelling
		library(brms)
		library(cmdstanr)
		library(tidybayes)
		library(collapse)
		# reporting
		library(quarto)
		library(knitr)
		library(gt)
		library(kableExtra)
		library(beeswarm)
	})
})

# load functions ---------------------------------------------------------------

invisible({ 
	lapply(list.files(path = c("R", "tests/testthat"),
					  full.names = TRUE, 
					  pattern = "\\.R"), source) 
})

# set params -------------------------------------------------------------------

options(mc.cores = 2,
		brms.backend = "cmdstanr",
		knitr.duplicate.label = "allow",
		cli.progress_bar_style = "dot")

tar_option_set(
	seed = 1234
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
	
	# get BVQ data -------------------------------------------------------------
	
	# this returns a list with all necessary data
	tar_target(bvq_data_file, 
			   file.path("data-raw", "stimuli", "bvq.rds"),
			   format = "file"),
	tar_target(bvq_data, readRDS(bvq_data_file)),
	
	# stimuli ------------------------------------------------------------------
	
	tar_target(trials_file, file.path("data-raw", "stimuli", "trials.xlsx"),
			   format = "file"),
	tar_target(trials, readxl::read_xlsx(trials_file)),
	tar_target(words_file, 
			   file.path("data-raw", "stimuli", "words.xlsx"),
			   format = "file"),
	tar_target(words, readxl::read_xlsx(words_file)),
	
	tar_target(childes, get_childes_corpora(words$childes_lemma, "eng")),
	tar_target(frequencies, get_frequency_childes(childes, words$childes_lemma)),
	tar_target(durations, get_audio_duration(trials)),
	tar_target(stimuli, get_stimuli(trials, words, frequencies, durations)), 
	
	# participants -------------------------------------------------------------
	
	tar_target(participants_file_bcn, 
			   file.path("data-raw", "participants", "participants-bcn.csv"),
			   format = "file"),
	
	tar_target(participants_file_oxf, 
			   file.path("data-raw", "participants", "participants-oxf.csv"),
			   format = "file"),
	
	tar_target(participants, 
			   get_participants(participants_file_bcn, participants_file_oxf)),
	
	# vocabulary ---------------------------------------------------------------
	
	tar_target(vocabulary_file_oxf, 
			   file.path("data-raw", "vocabulary", "vocabulary-oxf.xlsx"),
			   format = "file"),
	
	tar_target(vocabulary_supp_bcn_file,
			   list.files(file.path("data-raw", "vocabulary"),
			   		   pattern = "supp",
			   		   full.names = TRUE),
			   format = "file"),
	
	tar_target(vocabulary,
			   get_vocabulary(participants = participants, 
			   			   bvq_data = bvq_data,
			   			   vocabulary_supp_bcn_file,
			   			   vocabulary_file_oxf,
			   			   words)),
	
	# gaze data ----------------------------------------------------------------
	
	tar_target(aoi_coords,
			   list(
			   	c = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			   	l = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			   	r = c(xmin = 1240, xmax = 1740, ymin = 290, ymax = 790)
			   )),
	
	# Barcelona gaze data
	tar_target(files_bcn, 
			   list.files(
			   	path = file.path("data-raw", "eyetracking-bcn"), 
			   	pattern = ".csv$",
			   	full.names = TRUE
			   ), 
			   format = "file"),
	
	# Oxford gaze data
	tar_target(files_oxf,
			   list.files(
			   	file.path("data-raw", "eyetracking-oxf"), 
			   	pattern = ".csv$",
			   	full.names = TRUE
			   ),
			   format = "file"),
	
	tar_target(gaze, 
			   get_gaze(files_bcn, files_oxf, participants,
			   		 stimuli, aoi_coords, non_aoi_as_na = TRUE)),
	
	# attrition ----------------------------------------------------------------
	
	# Barcelona
	tar_target(attrition_trials,
			   get_attrition_trials(gaze, participants, stimuli, vocabulary,
			   					 vocabulary_by = "none",
			   					 aoi_coords = aoi_coords,
			   					 min_looking = c(prime = 0.75,
			   					 				test = 1.00,
			   					 				test_each = 0.10,
			   					 				test_any = 0.00))),
	
	tar_target(attrition_participants,
			   get_attrition_participants(attrition_trials,
			   						   vocabulary,
			   						   min_trials = c(cognate = 2,
			   						   			   noncognate = 2,
			   						   			   unrelated = 2),
			   						   min_l1_vocab = 0.1)),
	
	
	# Prepare for modelling data -----------------------------------------------
	
	tar_target(data_bcn,
			   get_data(gaze = filter(gaze, location=="Barcelona"),
			   		 participants, stimuli, vocabulary,
			   		 attrition_trials = attrition_trials,
			   		 attrition_participants = attrition_participants,
			   		 time_subset = c(0.30, 2.00))),
	
	tar_target(data_oxf,
			   {
			   	data_oxf <- get_data(gaze = filter(gaze, location=="Oxford"),
			   		 participants, stimuli, vocabulary,
			   		 attrition_trials = attrition_trials,
			   		 attrition_participants = attrition_participants,
			   		 time_subset = c(0.30, 2.00)) |> 
			   		mutate(condition = as.factor(if_else(condition != "Unrelated",
			   								   "Related", condition)))
			   	
			   	contrasts(data_oxf$condition) <- c(0.5, -0.5)
			   	
			   	return(data_oxf)
			   	}),
	
	# Bayesian GAMMs -----------------------------------------------------------
	
	tar_target(model_prior_bcn,
			   prior(normal(0, 0.5), class = "Intercept") +
			   	prior(normal(0, 0.5), class = "b") +
			   	prior(exponential(6), class = "sd") +
			   	prior(lkj(6), class = "cor") +
			   	prior(exponential(6), class = "sigma") +
			   	prior(exponential(6), class = "sds")),
	
	tar_target(model_formulas_bcn,
			   lst(
			   	fit_0 = .elog ~ condition * lp + age_std +
			   		s(timebin_std, bs = "bs", k = 8) +
			   		s(timebin_std, by = interaction(condition, lp), bs = "bs", k = 8) +
			   		(1 + condition + age_std | child_id) +
			   		(1 + condition | child_id:session_id),
			   	fit_1 = .elog ~ condition * lp * age_std +
			   		s(timebin_std, bs = "bs", k = 8) +
			   		s(timebin_std, by = interaction(condition, lp), bs = "bs", k = 8) +
			   		(1 + condition + age_std | child_id) +
			   		(1 + condition | child_id:session_id),
			   	fit_2 = .elog ~ condition * lp * voc_l1_std + 
			   		s(timebin_std, bs = "bs", k = 8) +
			   		s(timebin_std, by = interaction(condition, lp), bs = "bs", k = 8) +
			   		(1 + condition + voc_l1_std | child_id) +
			   		(1 + condition | child_id:session_id)
			   )),
	
	tar_target(model_names_bcn,
			   apply(
			   	expand.grid("fit_", seq(1, length(model_formulas_bcn))-1), 1,
			   	\(x) paste0(x[1], x[2])
			   )),
	
	tar_target(model_fits_bcn, get_model_fit(model_names_bcn,
											 model_formulas_bcn,
											 data_bcn,
											 model_prior_bcn)),
	
	tar_target(model_loos_bcn, get_model_loos(model_fits_bcn)),
	
	# Oxford models ------------------------------------------------------------
	
	tar_target(model_prior_oxf,
			   prior(normal(0, 0.5), class = "Intercept") +
			   	prior(normal(0, 0.5), class = "b") +
			   	prior(exponential(6), class = "sd") +
			   	prior(lkj(6), class = "cor") +
			   	prior(exponential(6), class = "sigma") +
			   	prior(exponential(6), class = "sds")),
	
	tar_target(model_formulas_oxf,
			   lst(
			   	fit_0 = .elog ~ condition + age_std +
			   		s(timebin_std, bs = "bs", k = 8) +
			   		s(timebin_std, by = condition, bs = "bs", k = 8) +
			   		(1 + condition + age_std | child_id) +
			   		(1 + condition | child_id:session_id),
			   	fit_1 = .elog ~ condition * age_std +
			   		s(timebin_std, bs = "bs", k = 8) +
			   		s(timebin_std, by = condition, bs = "bs", k = 8) +
			   		(1 + condition + age_std | child_id) +
			   		(1 + condition | child_id:session_id),
			   	fit_2 = .elog ~ condition * voc_l1_std + 
			   		s(timebin_std, bs = "bs", k = 8) +
			   		s(timebin_std, by = condition, bs = "bs", k = 8) +
			   		(1 + condition + voc_l1_std | child_id) +
			   		(1 + condition | child_id:session_id)
			   )),
	
	tar_target(model_names_oxf,
			   apply(
			   	expand.grid("fit_oxf_", seq(1, length(model_formulas_oxf))-1), 1,
			   	\(x) paste0(x[1], x[2])
			   )),
	
	tar_target(model_fits_oxf, get_model_fit(model_names_oxf,
											 model_formulas_oxf,
											 data_oxf,
											 model_prior_oxf)),
	
	tar_target(model_loos_oxf, get_model_loos(model_fits_oxf)),
	
	
	
	# render report ------------------------------------------------------------
	
	# tar_quarto(name = index, path = "docs/index.qmd")
	
	tar_quarto(manuscript,
			   file.path("manuscript", "manuscript.qmd"),
			   execute = TRUE,
			   quiet = FALSE)
	
)


