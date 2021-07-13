library(targets)

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
		"purrr", "eyetrackingR", "lme4", "lmerTest", "job"
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
	# run functions
	tar_target(
		attrition,
		get_attrition(
			participants = participants,
			vocabulary = vocabulary,
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			looking_threshold = c(prime = 250, target = 250), # minimum looking time
			missing_trials_threshold = c(cognate = 0, noncognate = 0, unrelated = 0) # minimum n trials in each condition
			
		)
	),
	
	# prepare data for analysis ----
	tar_target(
		prepared,
		prepare_data(
			gaze_bcn = gaze_bcn,
			gaze_oxf = gaze_oxf,
			participants = participants,
			stimuli = stimuli, 
			vocabulary = vocabulary,
			attrition = attrition
		)
	),
	
	# fit models ----
	# run functions
	tar_target(
		fit,
		fit_main_model(data = prepared)
	),
	tar_target(
		fit_21_mon,
		fit_21_mon_model(data = prepared)
	),
	tar_target(
		fit_21_mon_vocab_split,
		fit_21_mon_vocab_split_model(prepared, vocabulary)
	),
	tar_target(
		fit_21_mon_vocab_balanced,
		fit_21_mon_vocab_balanced_model(prepared, participants, vocabulary)
	),
	tar_target(
		fit_21_mon_location,
		fit_21_mon_location_model(data = prepared)
	)
	
)
