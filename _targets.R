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
source("tests/testthat/test-gaze.R")


# load packages ----
tar_option_set(
	packages = c(
		# project utils
		"arrow",
		"clustermq",
		"DiagrammeR",
		
	 
		"here",
		"conflicted",
		"usethis",
		"testthat",
		"lazyeval",
		# data manipulation
		"dplyr",
		"tidyr",
		"purrr", 
		"stringr",
		"tibble",
		"forcats", 
		"lubridate", 
		# data retrieval
		"janitor",
		"childesr",
		"multilex", 
		"keyring",
		"googlesheets4",
		"httr",
		# modelling
		"eyetrackingR",
		"brms",
		"tidybayes", 
		"emmeans",
		"mice",
		# data visualisation
		"ggplot2", 
		"shiny", 
		"patchwork",
		"shiny",
		"ggsci",
		# data reporting
		"knitr", 
		"papaja",
		"gt",
		# unit testing
		"readxl", 
		"rmarkdown", 
		"scales", 
		"shiny",
		"testthat",
		"targets",
		"tarchetypes",
		"tidytext",
		"zoo"
		
	)
)



# set params ----
options(
	mc.cores = 2,
	brms.backend = "cmdstanr",
	knitr.duplicate.label = "allow",
	clustermq.scheduler = "multiprocess"
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
	
	tar_target(
		resolve_conficts,
		{
			# resolve namespace conflicts ----
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
	
	# get multilex data (Barcelona vocabulary data) ----
	# log into multilex
	tar_target(
		credentials, 
		get_credentials()
	), # see R/utils.R
	
	# this returns a list with all necessary data
	tar_target(
		multilex_data, 
		get_multilex(
			update = TRUE,
			type = "understands"
		)
	), 
	
	# stimuli ----
	# import data
	tar_target(
		trials, 
		read_xlsx(here("stimuli", "stimuli.xlsx"))
	),
	tar_target(
		animacy, 
		here("data", "stimuli", "animacy.csv") %>% 
			read.csv() %>% 
			as_tibble() %>% 
			mutate(is_animate = as.logical(is_animate)) %>% 
			filter(test_language %in% c("Catalan", "Spanish"))
	),
	tar_target(
		familiarity, 
		get_familiarity(
			tokens = unique(unlist(distinct(trials, prime_cdi, target_cdi))),
			type = "understands", # defined in arguments
			update = update, # defined in arguments
			multilex_data = multilex_data # defined in arguments
		)
	),
	tar_target(
		childes,
		get_childes_corpora(
			token = unique(unlist(distinct(trials, prime, target))),
			languages = c("cat", "spa")
		)
	),
	tar_target(
		frequency_childes,
		get_frequency_childes(
			childes,
			token = unique(unlist(distinct(trials, prime, target)))
		)
	),
	tar_target(
		frequency_subtlex,
		get_frequency_subtlex(
			token = unique(unlist(distinct(trials, prime, target)))
		) 
	),
	tar_target(
		semantic_category,
		multilex_data$pool %>% # defined in arguments
			select(word = item, language, category) %>%
			rename(test_language = language)
	),
	# join all stimuli datasets into a single object
	# see src/R/00_stimuli.R for details on this function
	tar_target(
		stimuli, 
		get_stimuli(
			trials = trials,
			familiarity = familiarity,
			frequency_childes = frequency_childes,
			frequency_subtlex = frequency_subtlex,
			semantic_category = semantic_category,
			animacy = animacy, 
			
		)
	), 
	tar_target(
		stimuli_test,
		test_stimuli(stimuli) 
	),
	
	# participants ----
	
	# join datasets
	# see R/01_participants.R for details on this function
	tar_target(
		participants, 
		get_participants()
	),
	tar_target(
		participants_test,
		test_participants(participants)
	),
	
	# vocabulary ----
	
	tar_target(
		vocabulary,
		# see R/02_vocabulary.R for details on this function
		get_vocabulary(
			participants = participants, 
			update = FALSE,
			type = "understands",
			multilex_data = multilex_data
		)
	),
	tar_target(
		vocabulary_test,
		test_vocabulary(vocabulary)
	),
	
	# gaze data ----
	tar_target(
		gaze_files,
		get_gaze_files()
	),
	
	tar_target(
		aoi_coords,
		list(
			center = c(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
			left = c(xmin = 180, xmax = 680, ymin = 290, ymax = 790),
			right = c(xmin = 1240, xmax = 1640, ymin = 290, ymax = 790)
		)
	),
	
	# import data
	# see R/03_gaze_bcn.R and utils.R for details on this function
	tar_target(
		gaze_raw, 
		get_gaze_raw(
			participants = participants,
			stimuli = stimuli,
			aoi_coords = aoi_coords
		)
	),
	tar_target(
		gaze_raw_test,
		test_gaze_raw(gaze_raw)
	),
	tar_target(
		gaze_imputed, 
		impute_gaze(gaze_raw)
	),
	tar_target(
		gaze_imputed_test,
		test_gaze_imputed(gaze_imputed)
	),
	tar_target(
		gaze_plots,
		make_plots_gaze_raw(
			gaze_imputed,
			aoi_coords
		)
	),
	
	# attrition data ----
	# see R/04_attrition.R for details on this function
	tar_target(
		attrition,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			stimuli = stimuli,
			aoi_coords = aoi_coords,
			gaze_imputed = gaze_imputed,
			looking_threshold = c(
				prime = 0.25,
				target = 0.25,
				distractor = 0
			),
			missing_trials_threshold = c(
				cognate = 2, 
				non_cognate = 2,
				unrelated = 2
			),
			filter_vocabulary = NULL,
			filter_counterbalancing = FALSE
		)
	),
	tar_target(
		attrition_test,
		test_attrition(
			attrition,
			missing_trials_threshold = c(
				cognate = 2, 
				non_cognate = 2, 
				unrelated = 2
			)
		)
	),
	
	# prepare data for analysis ----
	# see R/05_prepare.R for details on the get_prepared() function
	# this function returns an analysis ready dataset, with all necessary (transformed/coded) variables
	tar_target(
		gaze,
		prepare_data(
			gaze_imputed = gaze_imputed,
			participants = participants,
			stimuli = stimuli, 
			vocabulary = vocabulary,
			attrition = attrition,
			aoi_coords = aoi_coords
		) %>% 
			filter(
				participant %in% unique(.$participant)[1:20]
			)
	),
	tar_target(
		gaze_test,
		test_gaze(gaze)
	),
	tar_target(
		gaze_aggregated,
		aggregate_data(gaze)
	),
	tar_target(
		gaze_processed_plots,
		make_plots_gaze_processed(gaze)
	),
	
	# see R/06_analysis.R for details on the fit_models() function
	# this function takes a list of formulas and list of datasets and fits a model 
	# that takes each formula-dataset pair at a time, and returns a named list of fits
	
	# set model prior
	tar_target(
		model_prior_agg,
		c(
			prior(normal(0, 0.1), class = "Intercept"),
			prior(normal(0, 0.1), class = "b"),
			prior(exponential(4), class = "sigma"),
			prior(exponential(4), class = "sd"),
			prior(lkj(5), class = "cor")
		)
	),
	
	# fit aggregated  models ----
	tar_target(
		fit_agg_0,
		{
			brm(
				logit ~ 1 + (1 | participant),
				prior = model_prior_agg[-c(2, 5),],
				data = gaze_aggregated,
				iter = 2000, chains = 2, seed = 888,
				save_model = here("stan", "fit_agg_0.stan")
			)
		}
	),
	tar_target(
		fit_agg_1,
		brm(
			logit ~ 1 + trial_type + (1 + trial_type | participant),
			prior = model_prior_agg,
			data = gaze_aggregated,
			iter = 2000, chains = 2, seed = 888,
			save_model = here("stan", "fit_agg_1.stan")
		)
	),
	tar_target(
		fit_agg_2,
		brm(
			logit ~ 1 + trial_type*lp + (1 + trial_type | participant),
			prior = model_prior,
			data = gaze_aggregated,
			iter = 2000, chains = 2, seed = 888,
			save_model = here("stan", "fit_agg_2.stan")
		)
	),
	tar_target(
		fit_agg_3,
		brm(
			logit ~ 1 + trial_type*lp*age_group + (1 + trial_type*age_group | participant),
			prior = model_prior_agg,
			data = gaze_aggregated,
			iter = 2000, chains = 2, seed = 888,
			save_model = here("stan", "fit_agg_3.stan")
		)
	),
	tar_target(
		fit_agg_4,
		brm(
			logit ~ 1 + trial_type*lp*vocab_size_total_center + (1 + trial_type*vocab_size_total_center | participant),
			prior = model_prior_agg,
			data = gaze_aggregated,
			iter = 2000, chains = 2, seed = 888,
			save_model = here("stan", "fit_agg_4.stan")
		)
	),
	tar_target(
		fit_agg_5,
		brm(
			logit ~ 1 + trial_type*lp*vocab_size_l1_center + (1 + trial_type*vocab_size_l1_center | participant),
			prior = model_prior_agg,
			data = gaze_aggregated,
			iter = 2000, chains = 2, seed = 888,
			save_model = here("stan", "fit_agg_5.stan")
		)
	),
	
	# compare aggregated models
	tar_target(
		model_fits_agg,
		lst(
			fit_agg_0,
			fit_agg_1,
			fit_agg_2,
			fit_agg_3,
			fit_agg_4,
			fit_agg_5
		)
	),
	
	tar_target(
		loos_agg,
		loo_compare(map(model_fits_agg, loo))
	),
	
	# fit growth curve analysis models ----
	
	# set model prior
	tar_target(
		model_prior,
		c(
			prior(normal(0, 0.1), class = "Intercept"),
			prior(normal(0, 0.1), class = "b"),
			prior(exponential(4), class = "sigma"),
			prior(exponential(4), class = "sd"),
			prior(lkj(5), class = "cor")
		)
	),
	
	tar_target(
		fit_0,
		brm(
			logit ~ 
				(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3)) +
				((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3)) | participant),
			prior = model_prior,
			data = gaze,
			iter = 2000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_0.stan")
		)
	),
	tar_target(
		fit_1,
		brm(
			logit ~ 
				(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type +
				((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type | participant),
			prior = model_prior,
			data = gaze,
			iter = 2000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_1.stan")
		)
	),
	tar_target(
		fit_2,
		brm(
			logit ~ 
				(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp +
				((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type | participant),
			prior = model_prior,
			data = gaze,
			iter = 2000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_2.stan")
		)
	),
	tar_target(
		fit_3,
		brm(
			logit ~ 
				(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp*age_group +
				((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp*age_group | participant),
			prior = model_prior,
			data = gaze,
			sample_prior = "yes",
			iter = 2000, chains = 2, seed = 888, init = 0,
			save_model = here("stan", "fit_3.stan"),
			fit = here("results", "fit_3.rds")
		)
	)
	# tar_target(
	# 	fit_1,
	# 	brm(
	# 		logit ~
	# 			(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type +
	# 			((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type | participant),
	# 		prior = model_prior,
	# 		data = gaze,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_1.stan")
	# 	)
	# ),
	# tar_target(
	# 	fit_2,
	# 	brm(
	# 		logit ~ 
	# 			(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp +
	# 			((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type | participant),
	# 		prior = model_prior,
	# 		data = gaze,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_2.stan")
	# 	)
	# ),
	# tar_target(
	# 	fit_3,
	# 	brm(
	# 		logit ~ 
	# 			(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*age_group +
	# 			((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*age_group | participant),
	# 		prior = model_prior,
	# 		data = gaze,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_3.stan")
	# 	)
	# ),
	# tar_target(
	# 	fit_4,
	# 	brm(
	# 		logit ~ 
	# 			(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*vocab_size_total_center +
	# 			((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*vocab_size_total_center | participant),
	# 		prior = model_prior,
	# 		data = gaze,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_4.stan")
	# 	)
	# ),
	# tar_target(
	# 	fit_5,
	# 	brm(
	# 		logit ~ 
	# 			(time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*vocab_size_l1_center +
	# 			((time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*vocab_size_l1_center | participant),
	# 		prior = model_prior,
	# 		data = gaze,
	# 		iter = 2000, chains = 2, seed = 888,
	# 		save_model = here("stan", "fit_5.stan")
	# 	)
	# ),
	
	# compare aggregated models
	# tar_target(
	# 	model_fits_agg,
	# 	lst(
	# 		fit_agg_0,
	# 		fit_agg_1,
	# 		fit_agg_2,
	# 		fit_agg_3,
	# 		fit_agg_4,
	# 		fit_agg_5
	# 	)
	# ),
	# 
	# tar_target(
	# 	loos_agg,
	# 	loo_compare(map(model_fits_agg, loo))
	# )
	
	# render docs
	# tar_render(docs_participants, "docs/00_participants.Rmd", priority = 0),
	# tar_render(docs_stimuli, "docs/01_stimuli.Rmd", priority = 0),
	# tar_render(docs_vocabulary, "docs/02_vocabulary.Rmd", priority = 0),
	# tar_render(docs_design, "docs/03_design.Rmd", priority = 0),
	# tar_render(docs_analysis, "docs/04_analysis.Rmd", priority = 0),
	# tar_render(docs_attrition, "docs/05_attrition.Rmd", priority = 0),
	# tar_render(docs_results, "docs/06_results.Rmd")
	
	# # render presentations
	# tar_render(communications_lacre_abstract, "presentations/2022-01-25_lacre/2022-01-25_lacre-abstract.Rmd", priority = 0),
	# tar_render(communications_lacre, "presentations/2022-01-25_lacre/2022-01-25_lacre.Rmd", priority = 0)
	#
	# # tar_render(communications_icis, "presentations/2022-07-07_icis/2022-07-07_icis-abstract.Rmd")
	
	# render manuscript
)


