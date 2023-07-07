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
	tar_target(bvq_data_file, "data-raw/bvq.rds", format = "file"),
	tar_target(bvq_data, readRDS(bvq_data_file)),
	
	# stimuli ------------------------------------------------------------------
	
	# import data
	tar_target(trials_file, "stimuli/stimuli.xlsx", format = "file"),
	tar_target(trials, readxl::read_xlsx(trials_file)),
	tar_target(childes_tokens, unique(unlist(distinct(trials, prime, target)))),
	tar_target(animacy, get_animacy()),
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
			   bvq_data$pool |> # defined in arguments
			   	select(word = item, language, semantic_category) |>
			   	rename(test_language = language)),
	# join all stimuli datasets into a single object
	# see src/R/00_stimuli.R for details on this function
	tar_target(
		stimuli, 
		get_stimuli(trials = trials,
					familiarity = familiarity,
					frequencies = frequencies,
					semantic_category = semantic_category,
					animacy = animacy)), 
	tar_target(stimuli_test, test_stimuli(stimuli)),
	
	# participants -------------------------------------------------------------
	
	# join datasets
	# see R/01_participants.R for details on this function
	tar_target(participants_file,
			   "data-raw/barcelona/participants.csv",
			   format = "file"),
	tar_target(participants, get_participants(participants_file)),
	tar_target(participants_test, test_participants(participants)),
	
	# vocabulary ---------------------------------------------------------------
	
	tar_target(vocabulary,
			   # see R/02_vocabulary.R for details on this function
			   get_vocabulary(participants = participants, 
			   			   bvq_data = bvq_data)),
	tar_target(vocabulary_test, test_vocabulary(vocabulary)),
	
	# gaze data ----------------------------------------------------------------
	tar_target(aoi_coords,
			   list(center = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			   	 left = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			   	 right = c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790))),
	
	tar_target(gaze_files, 
			   list.files("data-raw/barcelona/eyetracking/", 
			   		   pattern = ".csv$",
			   		   full.names = TRUE),
			   format = "file"),
	
	tar_target(gaze_normalised, 
			   get_gaze_normalised(gaze_files)),
	
	tar_target(gaze_raw, 
			   get_gaze_raw(gaze_normalised)),
	
	tar_target(gaze_raw_test,
			   test_gaze_raw(gaze_raw)),
	
	tar_target(gaze_processed, 
			   get_gaze_processed(gaze_raw, participants)),
	
	tar_target(gaze_processed_test,
			   test_gaze_processed(gaze_processed)),
	
	tar_target(gaze_aoi,
			   get_gaze_aoi(gaze_processed,
			   			 participants,
			   			 stimuli, 
			   			 aoi_coords)),
	
	tar_target(gaze_aoi_test, 
			   test_gaze_aoi(gaze_aoi)),
	# 
	# tar_target(gaze_plots,
	# 		   make_plots_gaze(
	# 		   	gaze_aoi, 
	# 		   	aoi_coords,
	# 		   	participants,
	# 		   	stimuli,
	# 		   	attrition_trials,
	# 		   	attrition_participants
	# 		   )),
	
	# attrition data -----------------------------------------------------------
	# see R/04_attrition.R for details on this function
	tar_target(attrition_trials,
			   get_attrition_trials(participants = participants,
			   					 aoi_coords = aoi_coords,
			   					 gaze_aoi = gaze_aoi,
			   					 looking_threshold = c(prime = 0.75, 
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
	# see R/05_prepare.R for details on the get_prepared() function
	# this function returns an analysis ready dataset, with all necessary (transformed/coded) variables
	tar_target(data_time,
			   get_data_time(gaze_aoi = gaze_aoi,
			   			  participants = participants,
			   			  stimuli = stimuli, 
			   			  vocabulary = vocabulary,
			   			  attrition_trials = attrition_trials,
			   			  attrition_participants = attrition_participants,
			   			  aoi_coords = aoi_coords, 
			   			  time_subset = c(0.25, 2))),
	
	tar_target(data_time_test,
			   test_data_time(data_time)),
	
	tar_target(data_summary,
			   get_data_summary(data_time)),
	
	tar_target(data_summary_test,
			   test_data_summary(data_summary)),
	
	tar_target(data_time_plots,
			   make_plots_gaze_processed(data_time, attrition_participants)),
	
	# analyse aggregated data
	tar_target(fit_aggregated_0,
			   get_model_fit(
			   	formula = .logit ~ age_group +
			   		(1 + age_group | id),
			   	data = data_summary,
			   	prior = c(prior(normal(0, 1), "Intercept"),
			   			  prior(normal(0, 1), "b"),
			   			  prior(normal(0, 1), "sd"),
			   			  prior(normal(0, 1), "sigma")),
			   	file = "results/fit_aggregated_0.rds"
			   )),
	
	tar_target(fit_aggregated_1,
			   get_model_fit(
			   	formula = .logit ~ age_group * lp +
			   		(1 + age_group * trial_type | id),
			   	data = data_summary,
			   	prior = c(prior(normal(0, 1), "Intercept"),
			   			  prior(normal(0, 1), "b"),
			   			  prior(normal(0, 1), "sd"),
			   			  prior(normal(0, 1), "sigma"),
			   			  prior(lkj(5), "cor")),
			   	file = "results/fit_aggregated_1.rds"
			   )),
	
	tar_target(fit_aggregated_2,
			   get_model_fit(
			   	formula = .logit ~ age_group * lp * trial_type +
			   		(1 + age_group * trial_type | id),
			   	data = data_summary,
			   	prior = c(prior(normal(0, 1), "Intercept"),
			   			  prior(normal(0, 1), "b"),
			   			  prior(normal(0, 1), "sd"),
			   			  prior(normal(0, 1), "sigma"),
			   			  prior(lkj(5), "cor")),
			   	file = "results/fit_aggregated_2.rds"
			   )),
	
	
	tar_target(loos_aggregated,
			   loo_compare(map(lst(fit_aggregated_0,
			   					fit_aggregated_1,
			   					fit_aggregated_2),
			   				loo))),
	
	# TIMECOURSE
	tar_target(fit_timecourse_0,
			   get_model_fit(
			   	formula = .logit ~ (ot1 + ot2 + ot3) * age_group +
			   		(1 + (ot1 + ot2 + ot3) * age_group | id),
			   	data = data_time,
			   	prior = c(prior(normal(0, 1), "Intercept"),
			   			  prior(normal(0, 1), "b"),
			   			  prior(normal(0, 1), "sd"),
			   			  prior(normal(0, 1), "sigma"),
			   			  prior(lkj(5), "cor")),
			   	file = "results/fit_timecourse_0.rds"
			   )),
	
	tar_target(fit_timecourse_1,
			   get_model_fit(
			   	formula = .logit ~ (ot1 + ot2 + ot3) * age_group * lp +
			   		(1 + (ot1 + ot2 + ot3) * age_group | id),
			   	data = data_time,
			   	prior = c(prior(normal(0, 1), "Intercept"),
			   			  prior(normal(0, 1), "b"),
			   			  prior(normal(0, 1), "sd"),
			   			  prior(normal(0, 1), "sigma"),
			   			  prior(lkj(5), "cor")),
			   	file = "results/fit_timecourse_1.rds"
			   )),
	
	tar_target(fit_timecourse_2,
			   get_model_fit(
			   	formula = .logit ~ (ot1 + ot2 + ot3) * age_group * lp * trial_type +
			   		(1 + (ot1 + ot2 + ot3) * age_group * trial_type | id),
			   	data = data_time,
			   	prior = c(prior(normal(0, 1), "Intercept"),
			   			  prior(normal(0, 1), "b"),
			   			  prior(normal(0, 1), "sd"),
			   			  prior(normal(0, 1), "sigma"),
			   			  prior(lkj(5), "cor")),
			   	file = "results/fit_timecourse_2.rds"
			   )),
	
	tar_target(loos_timecourse,
			   loo_compare(map(lst(fit_timecourse_0,
			   					fit_timecourse_1,
			   					fit_timecourse_2),
			   				loo)))
	
	
	# # render report
	# tar_quarto(report,
	# 		   "docs/index.qmd",
	# 		   execute = TRUE,
	# 		   quiet = FALSE)
	
)


