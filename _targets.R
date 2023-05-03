library(targets)
library(tarchetypes)

# load functions ---------------------------------------------------------------
invisible({ 
	lapply(list.files(path = "R", 
					  full.names = TRUE, 
					  pattern = ".R"), source) 
	lapply(list.files(path = "tests/testthat",
					  full.names = TRUE, 
					  pattern = ".R"), source)
})

# load packages ----------------------------------------------------------------
tar_option_set(
	packages = c(
		# project utils
		"arrow",
		"brms",
		"bvq",
		"childesr",
		"cli",
		"conflicted",
		"dplyr",
		"eyetrackingR",
		"forcats", 
		"ggplot2", 
		"googlesheets4",
		"gt",
		"here",
		"httr",
		"janitor",
		"lazyeval",
		"lubridate", 
		"mice",
		"patchwork",
		"polypoly",
		"purrr", 
		"readxl", 
		"scales", 
		"shiny",
		"stringr",
		"targets",
		"tarchetypes",
		"testthat",
		"tibble",
		"tidybayes", 
		"tidytext",
		"tidyr",
		"zoo"
	)
)

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
	
	tar_target(resolve_conficts, resolve_conflicts()),
	
	# get BVQ data -------------------------------------------------------------
	
	# this returns a list with all necessary data
	tar_target(bvq_data, get_bvq(type = "understands")), 
	
	# stimuli ------------------------------------------------------------------
	
	# import data
	tar_target(trials, read_xlsx("stimuli/stimuli.xlsx")),
	
	tar_target(childes_tokens, unique(unlist(distinct(trials, prime, target)))),
	
	tar_target(animacy, {
		read.csv("data/stimuli/animacy.csv") |> 
			as_tibble() |> 
			mutate(is_animate = as.logical(is_animate)) |> 
			filter(test_language %in% c("Catalan", "Spanish"))
	}),
	
	tar_target(familiarity, 
			   get_familiarity(
			   	tokens = unique(unlist(distinct(trials, prime_cdi, target_cdi))),
			   	type = "understands", # defined in arguments
			   	bvq_data = bvq_data)), # defined in arguments
	
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
	tar_target(participants, get_participants()),
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
	
	tar_target(gaze_normalised, get_gaze_normalised()),
	tar_target(gaze_joint, get_gaze_joint()),
	tar_target(gaze_joint_test, test_gaze_joint(gaze_joint)),
	tar_target(gaze_processed, get_gaze_processed(participants, stimuli, aoi_coords)),
	tar_target(gaze_processed_test, test_gaze_processed(gaze_processed)),
	tar_target(gaze_imputed, get_gaze_imputed(maxgap = 20)),
	tar_target(gaze_imputed_test, test_gaze_imputed(gaze_imputed)),
	tar_target(gaze_aoi, get_gaze_aoi(participants, stimuli, aoi_coords)),
	tar_target(gaze_aoi_test, test_gaze_aoi(gaze_aoi)),
	
	# tar_target(gaze_plots, make_plots_gaze_raw(gaze_imputed, aoi_coords)),
	
	# attrition data -----------------------------------------------------------
	# see R/04_attrition.R for details on this function
	tar_target(attrition,
			   get_attrition(participants = participants,
			   			  vocabulary = vocabulary,
			   			  stimuli = stimuli,
			   			  aoi_coords = aoi_coords,
			   			  gaze_aoi = gaze_aoi,
			   			  looking_threshold = c(prime = 0.25, target = 0.25, distractor = 0),
			   			  min_trials = c(cognate = 2, non_cognate = 2, unrelated = 2))),
	tar_target(attrition_test,
			   test_attrition(
			   	attrition,
			   	missing_trials_threshold = c(cognate = 2, non_cognate = 2, unrelated = 2))),
	
	# prepare data for analysis ------------------------------------------------
	# see R/05_prepare.R for details on the get_prepared() function
	# this function returns an analysis ready dataset, with all necessary (transformed/coded) variables
	tar_target(df,
			   prepare_data(gaze_aoi = gaze_aoi,
			   			 participants = participants,
			   			 stimuli = stimuli, 
			   			 vocabulary = vocabulary,
			   			 attrition = attrition,
			   			 aoi_coords = aoi_coords, 
			   			 time_subset = c(0, 2))),
	tar_target(df_test, test_df(df)),
	tar_target(df_aggregated, aggregate_data(df)),
	# tar_target(df_processed_plots, make_plots_gaze_processed(df)),
	
	# see R/06_analysis.R for details on the fit_models() function
	# this function takes a list of formulas and list of datasets and fits a model 
	# that takes each formula-dataset pair at a time, and returns a named list of fits
	
	# set model prior
	tar_target(model_prior_agg,
			   c(prior(normal(0, 0.1), class = "Intercept"),
			     prior(exponential(4), class = "sigma"),
			     prior(exponential(4), class = "sd"),
			     prior(lkj(5), class = "cor"))),
	
	# fit aggregated  models ---------------------------------------------------
	# tar_target(
	# 	fit_agg_prior,
	# 	brm(logit ~ 1 + (1 | id),
	# 		prior = model_prior_agg[-c(2, 5),],
	# 		data = df_aggregated,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_agg_prior.stan"),
	# 		file = here("results", "fit_agg_prior.rds"))
	# ),
	# tar_target(
	# 	fit_agg_0,
	# 	brm(logit ~ 1 + (1 | id),
	# 		prior = model_prior_agg[-c(2, 5),],
	# 		data = df_aggregated,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_agg_0.stan"),
	# 		file = here("results", "fit_agg_0.rds"))
	# ),
	
	# compare aggregated models
	# tar_target(
	# 	model_fits_agg,
	# 	lst(fit_agg_0, fit_agg_1, fit_agg_2, fit_agg_3, fit_agg_4, fit_agg_5)
	# ),
	# 
	# tar_target(loos_agg, loo_compare(map(model_fits_agg, loo))),
	
	# fit growth curve analysis models -----------------------------------------
	
	# set model prior
	tar_target(model_prior,
			   c(prior(normal(0, 1), class = "Intercept"),
			     prior(normal(0, 1), class = "b"),
			     prior(exponential(4), class = "sigma"),
			     prior(exponential(4), class = "sd"),
			     prior(lkj(4), class = "cor"))),
	tar_target(fit_prior,
			   brm(prop ~ (ot1 + ot2 + ot3) + ((ot1 + ot2 + ot3) | id),
			   	data = df,
			   	prior = model_prior,
			   	sample_prior = "only",
			   	# file_refit = "always",
			   	iter = 1000, chains = 2, seed = 888, init = 0,
			   	save_model = here("stan", "fit_prior.stan"),
			   	file = here("results", "fit_prior.rds"))),
	tar_target(fit_0,
			   brm(logit ~ (ot1 + ot2 + ot3) + ((ot1 + ot2 + ot3) | id),
			   	data = df,
			   	prior = model_prior,
			   	# file_refit = "always",
			   	iter = 1000, chains = 2, seed = 888,
			   	save_model = here("stan", "fit_0.stan"),
			   	file = here("results", "fit_0.rds"))),
	tar_target(fit_1,
			   brm(logit ~ (ot1 + ot2 + ot3)*age_group + ((ot1 + ot2 + ot3)*age_group | id),
			   	data = df,
			   	prior = model_prior,
			   	# file_refit = "always",
			   	iter = 1000, chains = 2, seed = 888, init = 0,
			   	save_model = here("stan", "fit_1.stan"),
			   	file = here("results", "fit_1.rds"))),
	tar_target(fit_2,
			   brm(logit ~ (ot1 + ot2 + ot3)*age_group*lp + ((ot1 + ot2 + ot3)*age_group | id),
			   	data = df,
			   	prior = model_prior,
			   	# file_refit = "always",
			   	iter = 1000, chains = 2, seed = 888, init = 0,
			   	save_model = here("stan", "fit_2.stan"),
			   	file = here("results", "fit_2.rds"))),
	tar_target(fit_3,
			   brm(logit ~ (ot1 + ot2 + ot3)*age_group*lp*trial_type + ((ot1 + ot2 + ot3)*age_group*lp*trial_type | id),
			   	data = df,
			   	prior = model_prior,
			   	# file_refit = "always",
			   	iter = 1000, chains = 2, seed = 888, init = 0,
			   	save_model = here("stan", "fit_3.stan"),
			   	file = here("results", "fit_3.rds"))),
	tar_target(fit_4,
			   brm(logit ~ (ot1 + ot2 + ot3)*vocab_std*lp*trial_type + ((ot1 + ot2 + ot3)*vocab_std*lp*trial_type | id),
			   	data = df,
			   	prior = model_prior,
			   	# file_refit = "always",
			   	iter = 1000, chains = 2, seed = 888, init = 1,
			   	save_model = here("stan", "fit_4.stan"),
			   	file = here("results", "fit_4.rds"))),
	
	# compare GCA models
	tar_target(loos, loo_compare(map(lst(fit_0, fit_1, fit_2, fit_3, fit_4), loo_subsample)))
	
	# render report
	# tar_quarto(report, 
	# 		   "docs/index.qmd", 
	# 		   execute = TRUE,
	# 		   quiet = FALSE)
)


