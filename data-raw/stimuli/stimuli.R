# Process stimuli data

library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(stringi)
library(tuneR)
library(bvq)
library(tidyr)
library(stringr)

# set params -------------------------------------------------------------------

# words to retrieve properties for
tokens <- unique(unlist(distinct(stimuli_raw, prime_cdi, target_cdi)))

# should missing data be imputed
impute <- FALSE

# load raw data ----------------------------------------------------------------

stimuli_path <- file.path("stimuli", "stimuli.xlsx")
stimuli_raw <- read_xlsx(stimuli_path)

bvq_data <- readRDS(file.path("data-raw/bvq/bvq.rds"))

# load functions ---------------------------------------------------------------

# utils ------------------------------------------------------------------------

#' Adjusted proportion from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) {
	
	prop <- (y + 2) / (n + 4)
	
	return(prop)
}

#' Adjusted proportion SE from Gelman, Hill & Vehtari (2020)
prop_adj_se <- function(y, n) {
	
	prop <- prop_adj(y, n)
	se <- sqrt(prop * (1 - prop) / (n + 4))
	
	return(se)
	
}

#' Adjusted proportion CI from Gelman, Hill & Vehtari (2020)
prop_adj_ci <- function(y, n, conf = 0.95) {
	
	prop <- (y + 2) / (n + 4)
	se <- sqrt(prop * (1 - prop)/(n + 4))
	ci <-  prop + qnorm(c((1 - .width) / 2, (1 - (1 - .width) / 2))) * se
	ci[1] <- ifelse(ci[1] < 0, 0, ci[1]) # truncate at 0
	ci[2] <- ifelse(ci[2] > 1, 1, ci[2]) # truncate at 1
	
	return(ci)
}

# Get audio durations ----------------------------------------------------------

#' Get duration of a WAV file
get_duration <- function(audio_path) {
	if (!file.exists(audio_path)) cli::cli_abort("{audio_path} does not exist")
	sound <- readWave(audio_path) # extract wave
	sound.length <- round(length(sound@left) / sound@samp.rate, 2) # duration
}

# get dir paths and validate paths
audio.dir <- paste0("stimuli/sounds/sounds_", c("cat", "spa")) 
audio.dir.valid <- dir.exists(audio.dir)

if (!all(audio.dir.valid))  {
	missing.dir <- audio.dir[!audio.dir.valid]
	error.msg <- "director{?y/ies} {missing.dir} {?does/do} not exist"
	cli_abort(error.msg)
}

# get audio paths
audio.paths <- list.files(audio.dir,
						  full.names = TRUE, 
						  pattern = ".wav$") 

durations <- tibble(audio = basename(audio.paths),
					duration = map_dbl(audio.paths, get_duration),
					audio_path = audio.paths) |> 
	mutate(audio = stri_trans_general(audio, id = "Latin-ASCII"),
		   test_language = ifelse(grepl("sounds_cat", audio_path),
		   					   "Catalan", "Spanish")) |> 
	relocate(audio, duration, test_language, audio_path)


#' Get familiarity data --------------------------------------------------------

# production or comprehension?
type <- "understands"

# age range for familiarity norms (min-max)
age <- c(17, 19)

# confidence level of the estimates
.width <- 0.95

familiarity <- bvq_norms(bvq_data$participants,
						 bvq_data$responses, 
						 type = type,
						 age = age,
						 .width = .width) |> 
filter(item_dominance=="L1") |> 
	summarise(across(c(.sum, .n), sum), .by = "item") |> 
	mutate(familiarity = prop_adj(.sum, .n),
		   familiarity_se = prop_adj_se(.sum, .n)) |> 
	select(item, starts_with("familiarity")) |> 
	rename(word = item) |> 
	mutate(test_language = case_when(
		grepl("eng_", word) ~ "English",
		grepl("cat_", word) ~ "Catalan",
		grepl("spa_", word) ~ "Spanish")) |> 
	relocate(test_language, .before = word)

#' Get frequencies from CHILDES ------------------------------------------------



#' Compute lexical frequencies from CHILDES corpora
# WARNING: especial characters are not being handled very well
# get total number of tokens in each language
total_counts <- childesr::get_speaker_statistics() |>
	filter(grepl(paste(languages, collapse = "|"), language)) |>
	summarise(num_tokens = sum(num_tokens), .by = language) |>
	mutate(language = strsplit(language, " ")) |>
	unnest(cols = language) |>
	summarise(n = sum(num_tokens, na.rm = TRUE), .by = language)

# relative frequency (counts per million)
lang_dict <- c(eng = "English", spa = "Spanish", cat = "Catalan")

frequencies <- childes |>
	left_join(total_counts, by = "language") |>
	mutate(freq_per_million = freq_counts/n*1e6,
		   freq_zipf = log10(freq_per_million)+3,
		   language = str_replace_all(language, lang_dict)) |>
	rename(word = gloss,
		   test_language = language,
		   freq = freq_zipf) |>
	select(word, test_language, freq)

#' Get animacy data ------------------------------------------------------------

animacy <- file.path("data-raw", "stimuli", "animacy.xlsx") |>
	read_xlsx() |> 
	mutate(is_animate = as.logical(is_animate)) |> 
	filter(test_language %in% c("Catalan", "Spanish"))

# Get semantic category --------------------------------------------------------

semantic_category <- bvq_data$pool |>
	select(word = item, language, semantic_category) |>
	rename(test_language = language)

# Get stimuli data (join everything) -------------------------------------------

# join all datasets (not very elegant, but does the trick)
# prime and target data are joined separately to avoid duplicates
stimuli_joint <- stimuli_raw |> 
	mutate(audio = stri_trans_general(str = stimuli_raw$audio, 
									  id = "Latin-ASCII")) |> 
	left_join(semantic_category, by = c("target_cdi" = "word", "test_language")) |> 
	rename(semantic_category_prime = semantic_category) |> 
	left_join(semantic_category, by = c("prime_cdi" = "word", "test_language")) |> 
	rename(semantic_category_target = semantic_category) |> 
	left_join(frequencies, by = c("prime" = "word", "test_language")) |> 
	rename(freq_prime = freq) |>
	left_join(frequencies, by = c("target" = "word", "test_language")) |> 
	rename(freq_target = freq) |> 
	left_join(animacy, by = c("prime" = "object", "test_language")) |> 
	rename(is_animate_prime = is_animate) |> 
	left_join(animacy, by = c("target" = "object", "test_language")) |> 
	rename(is_animate_target = is_animate) |> 
	left_join(familiarity, c("prime_cdi" = "word", "test_language")) |> 
	rename_at(vars(starts_with("familiarity")), function(x) paste0(x, "_prime")) |> 
	left_join(familiarity, c("target_cdi" = "word", "test_language")) |>
	left_join(durations, by = join_by(test_language, audio)) |> 
	rename_with(\(x) paste0(x, "_target"), 
				c(starts_with("familiarity") & 
				  	!ends_with("_prime"))) |> 
	rename_with(\(x) gsub("prime_target", "prime", x), everything()) |> 
	filter(location=="Barcelona")

# impute data
if (impute){ # defined in arguments
	stimuli_imputed <- stimuli_joint |> 
		mice::mice(printFlag = FALSE) |> # predictive mean matching
		mice::complete() |> # get complete dataset
		as_tibble()
} else {
	stimuli_imputed <- stimuli_joint
}

# select and reorder relevant variables
stimuli_bcn <- stimuli_joint |> 
	select(trial, test_language, version, list, trial_type,
		   prime, target, distractor, audio, duration, target_location,
		   prime_cdi, target_cdi, distractor_cdi,
		   valid_trial, 
		   familiarity_prime, familiarity_target,
		   familiarity_se_prime, familiarity_se_target,
		   freq_prime, freq_target,
		   semantic_category_prime, semantic_category_target,
		   is_animate_prime, is_animate_target,
		   is_animate_prime, is_animate_target) |> 
	mutate(across(c(trial, list), as.integer),
		   across(starts_with("is_animate"), as.logical))

# test_stimuli(stimuli)

arrow::write_csv_arrow(stimuli_bcn, file.path("data", "stimuli.csv"))


# Oxford -----------------------------------------------------------------------

#' Get stimuli information
stimuli_oxf <- gaze_oxf |> 
		distinct(pick(matches("_stm"))) |> 
		left_join(stimuli_cdi, by = c("prime_stm" = "stimulus")) |> 
		rename_with(\(x) gsub("item_", "prime_", x)) |> 
		left_join(stimuli_cdi, by = c("target_stm" = "stimulus")) |> 
		rename_with(\(x) gsub("item_", "target_", x)) |> 
		left_join(stimuli_cdi, by = c("distractor_stm" = "stimulus")) |> 
		rename_with(\(x) gsub("item_", "distractor_", x)) |> 
		mutate(trial_id = row_number()) |> 
		relocate(trial_id)
	
	return(stimuli)
}