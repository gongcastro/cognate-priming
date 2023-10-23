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
					  pattern = ".R"), source) 
})

# set params -------------------------------------------------------------------

options(mc.cores = 2,
		brms.backend = "cmdstanr",
		knitr.duplicate.label = "allow",
		cli.progress_bar_style = "dot")

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
	tar_target(bvq_data_file, file.path("data-raw", "bvq.rds"), format = "file"),
	tar_target(bvq_data, readRDS(bvq_data_file)),
	
	# stimuli ------------------------------------------------------------------
	
	# import data
	tar_target(trials_file, file.path("stimuli", "stimuli.xlsx"), format = "file"),
	tar_target(trials, readxl::read_xlsx(trials_file)),
	tar_target(childes_tokens, unique(unlist(distinct(trials, prime, target)))),
	tar_target(animacy_file, file.path("data-raw", "animacy.csv"), format = "file"),
	tar_target(animacy, get_animacy(animacy_file)),
	tar_target(familiarity, 
			   get_familiarity(
			   	tokens = unique(unlist(distinct(trials, prime_cdi, target_cdi))),
			   	type = "understands",
			   	bvq_data = bvq_data)),
	tar_target(childes, 
			   get_childes_corpora(
			   	token = childes_tokens,
			   	languages = c("cat", "spa"))),
	tar_target(frequencies,
			   get_frequency_childes(
			   	childes,
			   	token = childes_tokens)),
	tar_target(semantic_category,
			   bvq_data$pool |>
			   	select(word = item, language, semantic_category) |>
			   	rename(test_language = language)),
	tar_target(duration, get_audio_duration(trials)),
	# join all stimuli datasets
	tar_target(
		stimuli, 
		get_stimuli(trials = trials,
					familiarity = familiarity,
					frequencies = frequencies,
					semantic_category = semantic_category,
					animacy = animacy,
					duration = duration)), 
	
	# Oxford stimuli
	tar_target(stimuli_cdi_file_oxf,
			   file.path("data-raw", "stimuli-cdi-oxford.csv"),
			   format = "file"),
	tar_target(stimuli_cdi_oxf,
			   read_csv(stimuli_cdi_file_oxf, show_col_types = FALSE)),
	tar_target(stimuli_oxf, get_stimuli_oxf(gaze_processed_oxf, stimuli_cdi_oxf)),
	
	# participants -------------------------------------------------------------
	
	# Barcelona participants
	tar_target(participants_bcn_file, 
			   file.path("data-raw", "participants", "participants-barcelona.csv"),
			   format = "file"),
	tar_target(participants_oxf_file, 
			   file.path("data-raw", "participants", "participants-oxford.csv"),
			   format = "file"),
	tar_target(participants, get_participants(participants_bcn_file,
											  participants_oxf_file)),

	# vocabulary ---------------------------------------------------------------
	
	# Barcelona vocabulary 
	tar_target(vocabulary,
			   get_vocabulary(participants = participants, 
			   			   bvq_data = bvq_data)),
	
	# Oxford vocabulary
	tar_target(vocabulary_file_oxf,
			   file.path("data-raw", "participants", "Oxford_ESRC_participant.xlsx"),
			   format = "file"),
	tar_target(vocabulary_oxf,
			   get_vocabulary_oxf(vocabulary_file_oxf, 
			   				   participants)),
	
	# gaze data ----------------------------------------------------------------
	tar_target(aoi_coords,
			   list(center = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			   	 left = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			   	 right = c(xmin = 1240, xmax = 1740, ymin = 290, ymax = 790))),
	
	# Barcelona
	tar_target(gaze_files_bcn, 
			   list.files(path = "data-raw/eyetracking-barcelona", 
			   		   pattern = "\\csv$",
			   		   full.names = TRUE), 
			   format = "file"),
	
	tar_target(gaze_bcn, get_gaze_bcn(gaze_files_bcn, participants, stimuli, aoi_coords)),
	
	# Oxford
	tar_target(gaze_files_oxf,
			   list.files("data-raw/eyetracking-oxford", 
			   		   pattern = "\\.csv",
			   		   full.names = TRUE),
			   format = "file"),
	
	tar_target(gaze_oxf, get_gaze_oxf(gaze_files_oxf, participants)),
	
	# attrition ----------------------------------------------------------------
	
	# Barcelona
	tar_target(attrition_trials,
			   get_attrition_trials(
			   	participants = participants,
			   	vocabulary = vocabulary,
			   	vocabulary_by = c("prime", "target"),
			   	aoi_coords = aoi_coords,
			   	gaze = gaze_bcn,
			   	min_looking = c(prime = 0.75, test = 1, test_each = 0.1)
			   )
	),
	
	tar_target(attrition_participants,
			   get_attrition_participants(
			   	attrition_trials,
			   	min_trials = c(cognate = 2, noncognate = 2, unrelated = 2))
	),
	
	# Oxford
	tar_target(attrition_trials_oxf,
			   get_attrition_trials_oxf(
			   	participants = participants_oxf,
			   	stimuli = stimuli_oxf,
			   	vocabulary = vocabulary_oxf,
			   	vocabulary_by = c("prime", "target"),
			   	aoi_coords = aoi_coords_oxf,
			   	gaze_processed = gaze_processed_oxf,
			   	min_looking = c(
			   		prime = 0.75, 
			   		test = 1,
			   		test_each = 0.1))),
	
	tar_target(attrition_participants_oxf,
			   get_attrition_participants_oxf(attrition_trials_oxf,
			   							   min_trials = c(cognate = 2, 
			   							   			   noncognate = 2,
			   							   			   unrelated = 2))),
	
	# Modelling data -----------------------------------------------------------
	
	# Barcelona 
	tar_target(data_time_related,
			   get_data_time(gaze_aoi = gaze_aoi,
			   			  participants = participants,
			   			  stimuli = stimuli, 
			   			  vocabulary = vocabulary,
			   			  attrition_trials = attrition_trials,
			   			  attrition_participants = attrition_participants,
			   			  time_subset = c(0.3, 2),
			   			  contrast = "related")),
	
	tar_target(data_time_cognate,
			   get_data_time(gaze_aoi = gaze_aoi,
			   			  participants = participants,
			   			  stimuli = stimuli, 
			   			  vocabulary = vocabulary,
			   			  attrition_trials = attrition_trials,
			   			  attrition_participants = attrition_participants,
			   			  time_subset = c(0.3, 2),
			   			  contrast = "cognate")),
	
	# Oxford
	tar_target(data_time_related_oxf,
			   get_data_time_oxf(gaze_processed = gaze_processed_oxf,
			   				  participants = participants_oxf,
			   				  stimuli = stimuli_oxf, 
			   				  vocabulary = vocabulary_oxf,
			   				  attrition_trials = attrition_trials_oxf,
			   				  attrition_participants = attrition_participants_oxf,
			   				  time_subset = c(0.3, 2),
			   				  contrast = "related")),
	
	tar_target(data_time_cognate_oxf,
			   get_data_time_oxf(gaze_processed = gaze_processed_oxf,
			   				  participants = participants_oxf,
			   				  stimuli = stimuli_oxf, 
			   				  vocabulary = vocabulary_oxf,
			   				  attrition_trials = attrition_trials_oxf,
			   				  attrition_participants = attrition_participants_oxf,
			   				  time_subset = c(0.3, 2),
			   				  contrast = "cognate")),
	
	
	# Modelling ----------------------------------------------------------------
	tar_target(model_prior,
			   prior(normal(0, 0.5), class = "Intercept") +
			   	prior(normal(0, 0.5), class = "b") +
			   	prior(exponential(6), class = "sd") +
			   	prior(lkj(6), class = "cor") +
			   	prior(exponential(6), class = "sds")),
	
	tar_target(model_formulas,
			   list(
			   	# model_0 = .sum | trials(.nsamples) ~ 1 + lp + age + 
			   	# 	s(timebin, bs = "cr", k = 10) +
			   	# 	s(timebin, by = lp, bs = "cr", k = 10) + 
			   	# 	(1 + timebin + age | id),
			   	# model_1 = .sum | trials(.nsamples) ~ 1 + condition + age + 
			   	# 	s(timebin, bs = "cr", k = 10) +
			   	# 	s(timebin, by = condition, bs = "cr", k = 10) + 
			   	# 	(1 + timebin + age + condition | id),
			   	# model_2 = .sum | trials(.nsamples) ~ 1 + condition + lp + age + 
			   	# 	s(timebin, bs = "cr", k = 10) +
			   	# 	s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) + 
			   	# 	(1 + timebin + age + condition | id),
			   	model_3 = .sum | trials(.nsamples) ~ 1 + condition * lp + age + 
			   		s(timebin, bs = "cr", k = 10) +
			   		s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) + 
			   		(1 + timebin + age + condition | id)
			   )),
	
	tar_target(model_names, 
			   list(
			   	related = apply(expand.grid("fit_related_", 
			   								seq(1, length(model_formulas))-1), 1,
			   					\(x) paste0(x[1], x[2])),
			   	cognate = apply(expand.grid("fit_cognate_",
			   								seq(1, length(model_formulas))-1), 1, 
			   					\(x) paste0(x[1], x[2]))
			   )),
	
	tar_target(model_fits_related,
			   get_model_fit(model_names$related,
			   			  model_formulas,
			   			  data = data_time_related,
			   			  family = binomial("logit"),
			   			  prior = model_prior)),
	
	# tar_target(model_loo_related,
	# 		   get_model_loos(model_fits_related)),
	
	# cognate vs. non-cognate models	
	tar_target(model_fits_cognate,
			   get_model_fit(model_names$cognate,
			   			  model_formulas,
			   			  data = data_time_cognate,
			   			  family = binomial("logit"),
			   			  prior = model_prior)),
	
	# tar_target(model_loo_cognate,
	# 		   get_model_loos(model_fits_cognate)),
	
	# appendix -----------------------------------------------------------------
	
	tar_target(looking_times, 
			   get_looking_times(gaze_aoi = gaze_aoi,
			   				  participants = participants,
			   				  stimuli = stimuli)),
	
	# render report ------------------------------------------------------------
	tar_quarto(manuscript,
			   file.path("manuscript", "manuscript.qmd"),
			   execute = TRUE,
			   quiet = FALSE)
	
)


