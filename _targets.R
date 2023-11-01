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
	
	# participants -------------------------------------------------------------
	
	tar_target(participants_file_bcn, 
			   file.path("data-raw", "participants-bcn.csv"),
			   format = "file"),
	
	tar_target(participants_file_oxf, 
			   file.path("data-raw", "participants-oxf.csv"),
			   format = "file"),
	
	tar_target(participants, 
			   get_participants(participants_file_bcn,
			   				 participants_file_oxf)),
	
	# vocabulary ---------------------------------------------------------------
	
	tar_target(vocabulary_file_oxf,
			   file.path("data-raw", "vocabulary-oxford.xlsx"),
			   format = "file"),
	
	tar_target(cdi_file_oxf,
			   file.path("data-raw", "stimuli-cdi-oxford.csv"),
			   format = "file"),
	
	tar_target(vocabulary_supp_bcn_file,
			   file.path(list.files("data-raw",
			   					 pattern = "supp",
			   					 full.names = TRUE)),
			   format = "file"),
	
	tar_target(vocabulary,
			   get_vocabulary(participants = participants, 
			   			   bvq_data = bvq_data,
			   			   vocabulary_supp_bcn_file,
			   			   vocabulary_file_oxf,
			   			   cdi_file_oxf)),
	
	# gaze data ----------------------------------------------------------------
	
	tar_target(aoi_coords,
			   list(
			   	c = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			   	l = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			   	r = c(xmin = 1240, xmax = 1740, ymin = 290, ymax = 790)
			   )),
	
	# Barcelona gaze data
	tar_target(gaze_files_bcn, 
			   list.files(path = "data-raw/eyetracking-barcelona", 
			   		   pattern = ".csv$",
			   		   full.names = TRUE), 
			   format = "file"),
	
	# Oxford gaze data
	tar_target(gaze_files_oxf,
			   list.files("data-raw/eyetracking-oxford/", 
			   		   pattern = ".csv",
			   		   full.names = TRUE),
			   format = "file"),
	
	tar_target(gaze, 
			   get_gaze(gaze_files_bcn,
			   		 gaze_files_oxf,
			   		 participants,
			   		 aoi_coords,
			   		 stimuli,
			   		 non_aoi_as_na = TRUE)),
	
	# attrition ----------------------------------------------------------------
	
	# Barcelona
	tar_target(attrition_trials,
			   get_attrition_trials(
			   	participants = participants,
			   	vocabulary = vocabulary,
			   	vocabulary_by = "none",
			   	aoi_coords = aoi_coords,
			   	gaze = gaze,
			   	min_looking = c(prime = 0.75,
			   					test = 1.00,
			   					test_each = 0.00,
			   					test_any = 0.1))),
	
	tar_target(attrition_participants,
			   get_attrition_participants(attrition_trials,
			   						   min_trials = c(cognate = 2,
			   						   			   noncognate = 2,
			   						   			   unrelated = 2))),
	
	
	# Modelling data -----------------------------------------------------------
	
	tar_target(data_aggr,
			   get_data_aggr(gaze = gaze,
			   			  participants = participants,
			   			  stimuli = stimuli,
			   			  vocabulary = vocabulary,
			   			  attrition_trials = attrition_trials,
			   			  attrition_participants = attrition_participants,
			   			  time_subset = c(0.20, 2.00))),
	
	tar_target(data_time,
			   get_data_time(gaze = gaze,
			   			  participants = participants,
			   			  stimuli = stimuli,
			   			  vocabulary = vocabulary,
			   			  attrition_trials = attrition_trials,
			   			  attrition_participants = attrition_participants,
			   			  time_subset = c(0.20, 2.00),
			   			  return_clean = TRUE)),
	
<<<<<<< HEAD
	# 
	# # Model aggregated data ----------------------------------------------------
	# 
	# tar_target(model_prior,
	# 		   prior(normal(0, 0.5), class = "Intercept") +
	# 		   	prior(normal(0, 0.5), class = "b") +
	# 		   	prior(exponential(6), class = "sd") +
	# 		   	prior(lkj(6), class = "cor") +
	# 		   	prior(exponential(6), class = "sigma")),
	# 
	# tar_target(model_formulas_aggr,
	# 		   lst(
	# 		   	.elog ~ condition * lp + age + (1 + condition | session_id),
	# 		   	.elog ~ condition * lp + voc_l1 + (1 + condition | session_id),
	# 		   	.elog ~ condition + lp + voc_total + (1 + condition | session_id)
	# 		   )),
	# 
	# tar_target(model_names_aggr,
	# 		   apply(expand.grid("fit_aggr_", 
	# 		   				  seq(1, length(model_formulas_aggr))-1), 1,
	# 		   	  \(x) paste0(x[1], x[2]))),
	# 
	# tar_target(model_fits_aggr,
	# 		   get_model_fit(model_names_aggr,
	# 		   			  model_formulas_aggr,
	# 		   			  data = data_aggr,
	# 		   			  prior = model_prior)),
	# 
	# tar_target(model_loos_aggr,
	# 		   get_model_loos(model_fits_aggr)),
	# 
	# # Model time course data ---------------------------------------------------
	# 
	# tar_target(model_formulas_time,
	# 		   lst(
	# 		   	.elog ~ condition * lp * age +
	# 		   		s(timebin, bs = "cr", k = 10) +
	# 		   		s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) +
	# 		   		(1 + condition | session_id),
	# 		   	.elog ~ condition * lp * voc_l1 + 
	# 		   		s(timebin, bs = "cr", k = 10) +
	# 		   		s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) +
	# 		   		(1 + condition | session_id),
	# 		   	.elog ~ condition * lp * voc_total + 
	# 		   		s(timebin, bs = "cr", k = 10) +
	# 		   		s(timebin, by = interaction(condition, lp), bs = "cr", k = 10) +
	# 		   		(1 + condition | session_id)
	# 		   )),
	# 
	# tar_target(model_names_time,
	# 		   apply(expand.grid("fit_time_",
	# 		   				  seq(1, length(model_formulas_time))-1), 1,
	# 		   	  \(x) paste0(x[1], x[2])
	# 		   )),
	# 
	# tar_target(model_fits_time,
	# 		   get_model_fit(model_names_time,
	# 		   			  model_formulas_time,
	# 		   			  data = data_time,
	# 		   			  prior = model_prior +
	# 		   			  	prior(exponential(6), class = "sds"),
	# 		   )),
	# 
	# tar_target(model_loos_time,
	# 		   get_model_loos(model_fits_time)),
=======
	
	# Model time course data ---------------------------------------------------
	
	tar_target(model_prior,
			   prior(normal(0, 0.5), class = "Intercept") +
			   	prior(normal(0, 0.5), class = "b") +
			   	prior(exponential(6), class = "sd") +
			   	prior(lkj(6), class = "cor") +
			   	prior(exponential(6), class = "sigma") +
			   	prior(exponential(6), class = "sds")),
	
	tar_target(model_formulas,
			   lst(
			   	.elog ~ condition * lp * age_std +
			   		s(timebin_std, bs = "cr", k = 9) +
			   		s(timebin_std, by = interaction(condition, lp), bs = "cr", k = 9) +
			   		(1 + condition | session_id),
			   	.elog ~ condition * lp * voc_l1_std + 
			   		s(timebin_std, bs = "cr", k = 9) +
			   		s(timebin_std, by = interaction(condition, lp), bs = "cr", k = 9) +
			   		(1 + condition | session_id),
			   	.elog ~ condition * lp * voc_total_std + 
			   		s(timebin_std, bs = "cr", k = 9) +
			   		s(timebin_std, by = interaction(condition, lp), bs = "cr", k = 9) +
			   		(1 + condition | session_id)
			   )),
	
	tar_target(model_names,
			   apply(
			   	expand.grid("fit_", seq(1, length(model_formulas))-1), 1,
			   	\(x) paste0(x[1], x[2])
			   )),
	
	tar_target(model_fits,
			   get_model_fit(model_names,
			   			  model_formulas,
			   			  data = data_time,
			   			  prior = model_prior)),
	
	tar_target(model_loos, get_model_loos(model_fits)),
>>>>>>> 68c23d18b7c16a595af02b45d19333a1c5d15d2b
	
	
	# render report ------------------------------------------------------------
	
	# tar_quarto(name = index, path = "docs/index.qmd")
	
	tar_quarto(manuscript,
			   file.path("manuscript", "manuscript.qmd"),
			   execute = TRUE,
			   quiet = TRUE)
	
)


