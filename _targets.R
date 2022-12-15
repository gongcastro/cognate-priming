library(targets)
library(tarchetypes)

# load functions ----
source("R/utils.R")
source("R/00_stimuli.R")
source("R/01_participants.R")
source("R/02_vocabulary.R")
source("R/03_gaze.R")
source("R/04_attrition.R")
source("R/05_prepare.R")
source("R/06_analysis.R")

# load tests ----
source("tests/testthat/test-stimuli.R")
source("tests/testthat/test-participants.R")
source("tests/testthat/test-vocabulary.R")
source("tests/testthat/test-gaze_raw.R")
source("tests/testthat/test-gaze_imputed.R")
source("tests/testthat/test-attrition.R")
source("tests/testthat/test-df.R")


# load packages ----
tar_option_set(
	packages = c(
		# project utils
		"arrow",
		"brms",
		"bvqdev",
		"childesr",
		"cli",
		"conflicted",
		"dplyr",
		"eyetrackingR",
		"emmeans",
		"forcats", 
		"ggplot2", 
		"ggsci",
		"googlesheets4",
		"gt",
		"here",
		"httr",
		"janitor",
		"keyring",
		"knitr",
		"lazyeval",
		"lubridate", 
		"mice",
		"papaja",
		"patchwork",
		"polypoly",
		"purrr", 
		"readxl", 
		"rmarkdown", 
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
		clustermq.scheduler = "multiprocess",
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
	
	tar_target(
		resolve_conficts,
		{
			# resolve namespace conflicts --------------------------------------
			conflict_prefer("last_warnings", "rlang")
			conflict_prefer("filter", "dplyr")
			conflict_prefer("between", "dplyr")
			conflict_prefer("timestamp", "utils")
			conflict_prefer("ar", "brms")
			conflict_prefer("chisq.test", "stats")
			conflict_prefer("discard", "scales")
			conflict_prefer("duration", "lubridate")
			conflict_prefer("fisher.test", "stats")
			conflict_prefer("lag", "dplyr")
		}
	),
	
	# get BVQ data -------------------------------------------------------------
	
	# this returns a list with all necessary data
	tar_target(bvq_data, get_bvq(update = TRUE, type = "understands")), 
	
	# stimuli ------------------------------------------------------------------
	# import data
	tar_target(trials, read_xlsx("stimuli/stimuli.xlsx")),
	tar_target(animacy, 
			   read.csv("data/stimuli/animacy.csv") %>% 
			   	as_tibble() %>% 
			   	mutate(is_animate = as.logical(is_animate)) %>% 
			   	filter(test_language %in% c("Catalan", "Spanish"))),
	tar_target(
		familiarity, 
		get_familiarity(
			tokens = unique(unlist(distinct(trials, prime_cdi, target_cdi))),
			type = "understands", # defined in arguments
			update = update, # defined in arguments
			bvq_data = bvq_data # defined in arguments
		)
	),
	tar_target(
		childes,
		get_childes_corpora(token = unique(unlist(distinct(trials, prime, target))),
							languages = c("cat", "spa"))
	),
	tar_target(
		frequencies,
		get_frequency_childes(childes, token = unique(unlist(distinct(trials, prime, target))))
	),
	tar_target(
		semantic_category,
		bvq_data$pool %>% # defined in arguments
			select(word = item, language, semantic_category) %>%
			rename(test_language = language)
	),
	# join all stimuli datasets into a single object
	# see src/R/00_stimuli.R for details on this function
	tar_target(
		stimuli, 
		get_stimuli(trials = trials,
					familiarity = familiarity,
					frequencies = frequencies,
					semantic_category = semantic_category,
					animacy = animacy)
	), 
	tar_target(stimuli_test, test_stimuli(stimuli)),
	
	# participants -------------------------------------------------------------
	# join datasets
	# see R/01_participants.R for details on this function
	tar_target(participants, get_participants()),
	tar_target(participants_test, test_participants(participants)),
	
	# vocabulary ---------------------------------------------------------------
	tar_target(
		vocabulary,
		# see R/02_vocabulary.R for details on this function
		get_vocabulary(participants = participants, 
					   update = FALSE,
					   type = "understands",
					   bvq_data = bvq_data)
	),
	tar_target(vocabulary_test, test_vocabulary(vocabulary)),
	
	# gaze data ----------------------------------------------------------------
	tar_target(gaze_arrow, gaze_csv_to_arrow()),
	tar_target(gaze_files, get_gaze_files()),
	
	tar_target(
		aoi_coords,
		list(center = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			 left = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			 right = c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790))
	),
	
	# import data
	# see R/03_gaze_bcn.R and utils.R for details on this function
	tar_target(gaze_raw, get_gaze_raw(participants, stimuli, aoi_coords)),
	tar_target(gaze_raw_test, test_gaze_raw(gaze_raw)),
	tar_target(gaze_imputed, impute_gaze(gaze_raw, maxgap = 20, na.rm = FALSE)),
	tar_target(gaze_imputed_test, test_gaze_imputed(gaze_imputed)),
	# tar_target(gaze_plots, make_plots_gaze_raw(gaze_imputed, aoi_coords)),
	
	# attrition data -----------------------------------------------------------
	# see R/04_attrition.R for details on this function
	tar_target(
		attrition,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			stimuli = stimuli,
			aoi_coords = aoi_coords,
			gaze_imputed = gaze_imputed,
			looking_threshold = c(prime = 0.25, target = 0.25, distractor = 0),
			missing_trials_threshold = c(cognate = 2, non_cognate = 2, unrelated = 2),
			filter_vocabulary = NULL,
			filter_counterbalancing = FALSE
		)
	),
	tar_target(
		attrition_test,
		test_attrition(
			attrition,
			missing_trials_threshold = c(cognate = 2, 
										 non_cognate = 2, 
										 unrelated = 2))
	),
	
	# prepare data for analysis ------------------------------------------------
	# see R/05_prepare.R for details on the get_prepared() function
	# this function returns an analysis ready dataset, with all necessary (transformed/coded) variables
	tar_target(
		df,
		prepare_data(gaze_imputed = gaze_imputed,
					 participants = participants,
					 stimuli = stimuli, 
					 vocabulary = vocabulary,
					 attrition = attrition,
					 aoi_coords = aoi_coords, 
					 time_subset = c(0, 2)) 
	),
	tar_target(df_test, test_df(df)),
	tar_target(df_aggregated, aggregate_data(df)),
	# tar_target(df_processed_plots, make_plots_gaze_processed(df)),
	
	# see R/06_analysis.R for details on the fit_models() function
	# this function takes a list of formulas and list of datasets and fits a model 
	# that takes each formula-dataset pair at a time, and returns a named list of fits
	
	# set model prior
	tar_target(
		model_prior_agg,
		c(prior(normal(0, 0.1), class = "Intercept"),
		  
		  prior(exponential(4), class = "sigma"),
		  prior(exponential(4), class = "sd"),
		  prior(lkj(5), class = "cor"))
	),
	
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
	tar_target(
		model_prior,
		c(prior(normal(0, 1), class = "Intercept"),
		  prior(normal(0, 1), class = "b"),
		  prior(exponential(4), class = "sigma"),
		  prior(exponential(4), class = "sd"),
		  prior(lkj(4), class = "cor"))
	),
	tar_target(
		fit_prior,
		brm(prop ~ (ot1 + ot2 + ot3) + ((ot1 + ot2 + ot3) | id),
			data = df,
			prior = model_prior,
			sample_prior = "only",
			# file_refit = "always",
			iter = 1000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_prior.stan"),
			file = here("results", "fit_prior.rds"))
	),
	tar_target(
		fit_0,
		brm(logit ~ (ot1 + ot2 + ot3) + ((ot1 + ot2 + ot3) | id),
			data = df,
			prior = model_prior,
			# file_refit = "always",
			iter = 1000, chains = 2, seed = 888,
			save_model = here("stan", "fit_0.stan"),
			file = here("results", "fit_0.rds"))
	),
	tar_target(
		fit_1,
		brm(logit ~ (ot1 + ot2 + ot3)*age_group + ((ot1 + ot2 + ot3)*age_group | id),
			data = df,
			prior = model_prior,
			# file_refit = "always",
			iter = 1000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_1.stan"),
			file = here("results", "fit_1.rds"))
	),
	tar_target(
		fit_2,
		brm(logit ~ (ot1 + ot2 + ot3)*age_group*lp + ((ot1 + ot2 + ot3)*age_group | id),
			data = df,
			prior = model_prior,
			# file_refit = "always",
			iter = 1000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_2.stan"),
			file = here("results", "fit_2.rds"))
	),
	tar_target(
		fit_3,
		brm(logit ~ (ot1 + ot2 + ot3)*age_group*lp*trial_type + ((ot1 + ot2 + ot3)*age_group*lp*trial_type | id),
			data = df,
			prior = model_prior,
			# file_refit = "always",
			iter = 1000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_3.stan"),
			file = here("results", "fit_3.rds"))
	),
	tar_target(
		fit_4,
		brm(logit ~ (ot1 + ot2 + ot3)*vocab_std*lp*trial_type + ((ot1 + ot2 + ot3)*vocab_std*lp*trial_type | id),
			data = df,
			prior = model_prior,
			# file_refit = "always",
			iter = 1000, chains = 2, seed = 888, init = 1,
			save_model = here("stan", "fit_4.stan"),
			file = here("results", "fit_4.rds"))
	),
	
	# compare GCA models
	tar_target(loos, loo_compare(map(lst(fit_0, fit_1, fit_2, fit_3, fit_4), loo_subsample)))
	
	# render manuscript
)


