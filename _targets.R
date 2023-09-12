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
	# tar_target(stimuli_test, test_stimuli(stimuli)),
	
	# participants -------------------------------------------------------------
	
	# join datasets
	# see R/01_participants.R for details on this function
	tar_target(participants_file, 
			   file.path("data-raw", "participants.csv"),
			   format = "file"),
	tar_target(participants, get_participants(participants_file)),
	tar_target(participants_test, test_participants(participants)),
	
	# vocabulary ---------------------------------------------------------------
	
	tar_target(vocabulary,
			   get_vocabulary(participants = participants, 
			   			   bvq_data = bvq_data)),
	# tar_target(vocabulary_test, test_vocabulary(vocabulary)),
	
	# gaze data ----------------------------------------------------------------
	tar_target(aoi_coords,
			   list(center = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			   	 left = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			   	 right = c(xmin = 1240, xmax = 1740, ymin = 290, ymax = 790))),
	
	# gaze data ----
	tar_target(gaze_files, 
			   list.files(
			   	path = "data-raw/eyetracking", 
			   	pattern = ".csv$",
			   	full.names = TRUE
			   ), 
			   format = "file"),
	
	tar_target(gaze_raw, get_gaze_raw(gaze_files)),
	tar_target(gaze_raw_test, test_gaze_raw(gaze_raw)),
	
	tar_target(gaze_processed, get_gaze_processed(gaze_raw)),
	tar_target(gaze_processed_test, test_gaze_processed(gaze_processed)),
	
	tar_target(gaze_aoi, get_gaze_aoi(gaze_processed, participants, stimuli, aoi_coords)),
	tar_target(gaze_aoi_test, test_gaze_aoi(gaze_aoi)),
	
	# attrition ----------------------------------------------------------------
	
	tar_target(attrition_trials,
			   get_attrition_trials(
			   	participants = participants,
			   	vocabulary = vocabulary,
			   	vocabulary_by = c("prime", "target"),
			   	aoi_coords = aoi_coords,
			   	gaze_aoi = gaze_aoi,
			   	min_looking = c(
			   		prime = 0.75, 
			   		test = 1,
			   		test_each = 0.1))),
	
	tar_target(attrition_trials_test,
			   test_attrition_trials(attrition_trials)),
	
	tar_target(attrition_participants,
			   get_attrition_participants(attrition_trials,
			   						   min_trials = c(cognate = 2, 
			   						   			   noncognate = 2,
			   						   			   unrelated = 2))),
	tar_target(attrition_participants_test,
			   test_attrition_participants(attrition_participants)),
	
	# prepare data for analysis ------------------------------------------------
	
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
	
	# tar_target(data_time_test, test_data_time(data_time)),
	
	# tar_target(data_summary_related, get_data_summary(data_time)),
	# tar_target(data_summary_test, test_data_summary(data_summary)),
	
	# growth curve analysis
	tar_target(model_prior,
			   c(prior(normal(0, 0.5), "Intercept"),
			     prior(normal(0, 0.5), "b"),
			     prior(exponential(6), "sd"),
			     prior(lkj(6), "cor"),
			     prior(exponential(6), "sds"))),
	
	tar_target(model_formulas,
			   list(
			   	model_0 = .sum | trials(.nsamples) ~ 1 + lp + age + 
			   		s(timebin, bs = "cr", k = 10) +
			   		s(timebin, by = lp, bs = "cr", k = 10) + 
			   		(1 + timebin + age | id),
			   	model_1 = .sum | trials(.nsamples) ~ 1 + condition + age + 
			   		s(timebin, bs = "cr", k = 10) +
			   		s(timebin, by = condition, bs = "cr", k = 10) + 
			   		(1 + timebin + age + condition | id),
			   	model_2 = .sum | trials(.nsamples) ~ 1 + condition + lp + age + 
			   		s(timebin, bs = "cr", k = 10) +
			   		s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) + 
			   		(1 + timebin + age + condition | id),
			   	model_3 = .sum | trials(.nsamples) ~ 1 + condition * lp + age + 
			   		s(timebin, bs = "cr", k = 10) +
			   		s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) + 
			   		(1 + timebin + age + condition | id)
			   )),
	
	tar_target(model_fit_related_0,
			   get_model_fit(
			   	formula = model_formulas$model_0,
			   	family = binomial("logit"),
			   	data = data_time_related,
			   	prior = model_prior,
			   	file = file.path("results", "fit_related_0.rds")
			   )),
	
	tar_target(model_fit_related_1,
			   get_model_fit(
			   	formula = model_formulas$model_1,
			   	family = binomial("logit"),
			   	data = data_time_related,
			   	prior = model_prior,
			   	file = file.path("results", "fit_related_1.rds")
			   )),
	
	tar_target(model_fit_related_2,
			   get_model_fit(
			   	formula = model_formulas$model_2,
			   	family = binomial("logit"),
			   	data = data_time_related,
			   	prior = model_prior,
			   	file = file.path("results", "fit_related_2.rds")
			   )),
	
	tar_target(model_fit_related_3,
			   get_model_fit(
			   	formula = model_formulas$model_3,
			   	family = binomial("logit"),
			   	data = data_time_related,
			   	prior = model_prior,
			   	file = file.path("results", "fit_related_3.rds")
			   )),
	
	tar_target(model_loo_related,
			   get_model_loos(model_fit_related_0,
			   			   model_fit_related_1,
			   			   model_fit_related_2,
			   			   model_fit_related_3)),
	
	# cognate vs. non-cognate models	
	tar_target(model_fit_cognate_0,
			   get_model_fit(
			   	formula = model_formulas$model_0,
			   	family = binomial("logit"),
			   	data = data_time_cognate,
			   	prior = model_prior,
			   	file = file.path("results", "fit_cognate_0.rds")
			   )),
	
	tar_target(model_fit_cognate_1,
			   get_model_fit(
			   	formula = model_formulas$model_1,
			   	family = binomial("logit"),
			   	data = data_time_cognate,
			   	prior = model_prior,
			   	file = file.path("results", "fit_cognate_1.rds")
			   )),
	tar_target(model_fit_cognate_2,
			   get_model_fit(
			   	formula = model_formulas$model_2,
			   	family = binomial("logit"),
			   	data = data_time_cognate,
			   	prior = model_prior,
			   	file = file.path("results", "fit_cognate_2.rds")
			   )),
	tar_target(model_fit_cognate_3,
			   get_model_fit(
			   	formula = model_formulas$model_3,
			   	family = binomial("logit"),
			   	data = data_time_cognate,
			   	prior = model_prior,
			   	file = file.path("results", "fit_cognate_3.rds")
			   )),
	
	tar_target(model_loo_cognate,
			   get_model_loos(model_fit_cognate_0,
			   			   model_fit_cognate_1,
			   			   model_fit_cognate_2,
			   			   model_fit_cognate_3)),
	
	
	# render report ------------------------------------------------------------
	tar_quarto(report,
			   file.path("manuscript", "manuscript.qmd"),
			   execute = TRUE,
			   quiet = FALSE)
	
)


