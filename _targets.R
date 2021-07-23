library(targets)
library(tarchetypes)

source("R/utils.R")
source("R/00_stimuli.R")
source("R/01_participants.R")
source("R/02_vocabulary.R")
source("R/03_gaze_bcn.R")
source("R/03_gaze_oxf.R")
source("R/04_attrition.R")
source("R/05_prepare.R")
source("R/06_analysis.R")

# set parameters ----
tar_option_set(
	packages = c(
		"dplyr", "tidyr", "stringr", "multilex", "keyring",
		"readxl", "janitor", "childesr", "mice", "here",
		"googlesheets4", "lubridate", "httr", "data.table",
		"purrr", "eyetrackingR", "lme4", "lmerTest", "shiny",
		"rmarkdown", "knitr"
	)
)
options(tidyverse.quiet = TRUE)

# End this file with a list of target objects.
list(
	
	# get multilex data ----
	tar_target(credentials, get_credentials()),
	tar_target(multilex_data, get_multilex(update = TRUE, type = "understands")),
	
	# stimuli ----
	# define data
	tar_target(trials_path, here("Stimuli", "stimuli.xlsx"), format = "file"),
	tar_target(stimuli_english_path, here("Data", "Stimuli", "stimuli_english.xlsx"), format = "file"),
	tar_target(animacy_path, here("Data", "Stimuli", "animacy.csv"), format = "file"),
	
	# import data
	tar_target(trials, read_xlsx(trials_path)),
	tar_target(stimuli_english, read_xlsx(stimuli_english_path, na = "NA")),
	tar_target(animacy, read.csv(animacy_path)),
	
	# run functions
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
	# define data
	tar_target(
		participants_oxf_path,
		here("Data", "Participants", "participant_oxford_Apr2021.xlsx"),
		format = "file"
	),
	# import data
	tar_target(
		participants_bcn,
		range_read(
			ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE",
			sheet = "barcelona", na = ""
		)
	),
	tar_target(
		participants_oxf,
		read_xlsx(
			participants_oxf_path,
			na = "NA"
		)
	),
	# run functions
	tar_target(
		participants,
		get_participants(participants_bcn, participants_oxf)
	),
	
	# vocabulary ----
	# define data
	tar_target(
		vocabulary_oxf_path,
		here("Data", "Vocabulary", "vocabulary_oxford_Apr2021.xlsx")
	),
	# import data
	tar_target(
		vocabulary_oxf,
		vocabulary_oxf_path %>% 
			excel_sheets() %>%
			map(~read_xlsx(vocabulary_oxf_path, sheet = ., na = c("", "NA", "?", "x"))) %>%
			set_names(excel_sheets(vocabulary_oxf_path)) %>%
			bind_rows(.id = "version")
	),
	# run functions
	tar_target(
		vocabulary,
		get_vocabulary(
			participants = participants, 
			update = FALSE,
			type = "understands",
			impute = TRUE,
			vocabulary_oxf = vocabulary_oxf,
			multilex_data = multilex_data
		)
	),
	
	# gaze Barcelona ----
	# define data
	tar_target(
		gaze_bcn_paths,
		list.files(
			here("Data", "Gaze", "Barcelona"),
			full.names = TRUE)
	),
	
	# run functions
	tar_target(
		gaze_bcn,
		get_gaze_bcn(
			file_paths = gaze_bcn_paths,
			participants = participants,
			stimuli = stimuli
		)
	),
	
	# get Oxford ----
	# define data
	tar_target(
		gaze_oxf_paths,
		list.files(
			here("Data", "Gaze", "Oxford"),
			full.names = TRUE
		)
	),
	# run functions
	tar_target(
		gaze_oxf,
		get_gaze_oxf(
			file_paths = gaze_oxf_paths,
			participants = participants,
			stimuli = stimuli
		)
	),
	
	# get attrition ----
	# stringent version
	tar_target(
		attrition,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			looking_threshold = c(prime = 250, target = 250, distractor = 0), # minimum looking time
			missing_trials_threshold = c(cognate = 2, noncognate = 2, unrelated = 2), # minimum n trials in each condition
			filter_vocabulary = c("prime", "target"),
			filter_counterbalancing = FALSE
		)
	),
	# relaxed version
	tar_target(
		attrition_relaxed,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			looking_threshold = c(prime = 250, target = 250, distractor = 0), # minimum looking time
			missing_trials_threshold = c(cognate = 0, noncognate = 0, unrelated = 0), # minimum n trials in each condition
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
			missing_trials_threshold = c(cognate = 2, noncognate = 2, unrelated = 2), # minimum n trials in each condition
			filter_counterbalancing = TRUE,
			filter_vocabulary = c("prime", "target")
		)
	),
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
	
	# fit frequentist models ----
	tar_target(
		model_formulas,
		list(
			fit = "elog ~ age_group + vocab_size_l1_center*trial_type*lp*(ot1+ot2) + (1+ot1+ot2+ot3+trial_type+age_group | participant)",
			fit_21_mon = "elog ~  vocab_size_l1_center*trial_type*(ot1+ot2+ot3) + (1+ot1+ot2+ot3+trial_type | participant)",
			fit_21_mon_location = "elog ~ trial_type*location*(ot1+ot2+ot3) + (1+ot1+ot2 | participant)",
			fit_21_mon_vocab_split = "elog ~ trial_type*vocab_cat*(ot1+ot2+ot3) + (1+ot1+ot2 | participant)",
			fit_21_mon_vocab_balanced = "elog ~ trial_type*(ot1+ot2+ot3) + (1+ot1+ot2+ot3 | participant)"
		)
	),
	tar_target(
		model_datasets,
		list(
			fit = gaze,
			fit_21_mon = gaze %>% filter(age_group=="21 months", lp=="Monolingual"),
			fit_21_mon_location = gaze %>% filter(age_group=="21 months", lp=="Monolingual"),
			fit_21_mon_vocab_split = gaze %>%
				filter(age_group=="21 months", lp=="Monolingual") %>%
				mutate(vocab_cat = ifelse(
					vocab_size_l1_center > median(distinct(gaze_relaxed, participant, vocab_size_l1_center)$vocab_size_l1_center, na.rm = TRUE), 
					"Above median", 
					"Below median"
				)) %>% 
				mutate_at(vars(vocab_cat, location), as.factor),
			fit_21_mon_vocab_balanced = gaze %>%
				filter(
					age_group=="21 months",
					lp=="Monolingual",
					participant %in% {
						participants %>%
							left_join(vocabulary) %>% 
							drop_na(vocab_size_l1) %>% 
							filter(age_group=="21 months", lp=="Monolingual") %>% 
							arrange(location, desc(vocab_size_l1)) %>% 
							group_by(location) %>% 
							mutate(vocab_index = row_number()) %>% 
							ungroup() %>% 
							filter(
								((location=="Barcelona") & (vocab_index %in% 1:15)) |
									((location=="Oxford")) & (vocab_index %in% seq(
										max(vocab_index[location=="Oxford"])-15,
										max(vocab_index[location=="Oxford"]))
									)
							) %>%
							pull(participant)
					}
				)
		)
	),
	tar_target(
		model_datasets_relaxed,
		list(
			fit_relaxed = gaze_relaxed,
			fit_21_mon_relaxed = gaze_relaxed %>% filter(age_group=="21 months", lp=="Monolingual"),
			fit_21_mon_location_relaxed = gaze_relaxed %>% filter(age_group=="21 months", lp=="Monolingual"),
			fit_21_mon_vocab_split_relaxed = gaze_relaxed %>%
				filter(age_group=="21 months", lp=="Monolingual") %>%
				mutate(vocab_cat = ifelse(
					vocab_size_l1_center > median(distinct(gaze_relaxed, participant, vocab_size_l1_center)$vocab_size_l1_center, na.rm = TRUE), 
					"Above median", 
					"Below median"
				)) %>% 
				mutate_at(vars(vocab_cat, location), as.factor),
			fit_21_mon_vocab_balanced_relaxed = gaze_relaxed %>%
				filter(
					age_group=="21 months",
					lp=="Monolingual",
					participant %in% {
						participants %>%
							left_join(vocabulary) %>% 
							drop_na(vocab_size_l1) %>% 
							filter(age_group=="21 months", lp=="Monolingual") %>% 
							arrange(location, desc(vocab_size_l1)) %>% 
							group_by(location) %>% 
							mutate(vocab_index = row_number()) %>% 
							ungroup() %>% 
							filter(
								((location=="Barcelona") & (vocab_index %in% 1:15)) |
									((location=="Oxford")) & (vocab_index %in% seq(
										max(vocab_index[location=="Oxford"])-15,
										max(vocab_index[location=="Oxford"]))
									)
							) %>%
							pull(participant)
					}
				)
		)
		
	),
	tar_target(
		model_fits,
		fit_models(
			formulas = model_formulas,
			datasets = model_datasets
		)
	),
	tar_target(
		model_fits_relaxed,
		fit_models(
			formulas = model_formulas,
			datasets = model_datasets_relaxed
		)
	),
	# render Rmd
	tar_render(report, "Rmd/report.Rmd")
	
)
