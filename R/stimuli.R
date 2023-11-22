#' Get audio duration
get_audio_duration <- function(trials) {
	
	# get dir paths and validate paths
	audio.dir <- paste0("stimuli/sounds/sounds-", c("cat", "eng", "spa"))
	audio.dir.valid <- dir.exists(audio.dir)
	
	if (!all(audio.dir.valid)) {
		missing.dir <- audio.dir[!audio.dir.valid]
		error.msg <- "director{?y/ies} {missing.dir} {?does/do} not exist"
		cli_abort(error.msg)
	}
	
	# get audio paths
	audio.paths <- list.files(audio.dir, full.names = TRUE, pattern = ".wav$")
	durations.vctr <- purrr::map_dbl(audio.paths, get_duration)
	durations <- tibble::tibble(
		audio_path = audio.paths,
		duration = durations.vctr
	)
	durations$audio <- basename(audio.paths)
	durations$test_language <- case_when(
		grepl("-cat", audio.paths) ~ "Catalan",
		grepl("-spa", audio.paths) ~ "Spanish",
		grepl("-eng", audio.paths) ~ "English"
	)
	durations$duration <- if_else(
		durations$test_language=="English",
		durations$duration - 4, 
		durations$duration
	)
	
	# merge with trials dataset
	trials_tmp <- distinct(trials, test_language, version, audio)
	trials_tmp$audio <- stringi::stri_trans_general(
		str = trials_tmp$audio,
		id = "Latin-ASCII"
	)
	duration <- inner_join(trials_tmp, durations,
						   relationship = "many-to-many",
						   by = join_by(test_language, audio)
	)
	
	return(duration)
}

#' Get duration of a WAV file
get_duration <- function(audio_path) {
	if (!file.exists(audio_path)) cli::cli_abort("{audio_path} does not exist")
	sound <- tuneR::readWave(audio_path) # extract wave
	sound.length <- round(length(sound@left) / sound@samp.rate, 2) # duration
}

#' Get familiarity data
get_familiarity <- function(bvq_data,
							tokens, # words to retrieve familiarity for
							type = "understands", # understands or produces
							age = c(17, 19), # age range for familiarity norms (min-max)
							.width = 0.95 # confidence level of the estimates
) {
	familiarity <- bvq::bvq_norms(bvq_data$participants,
								  bvq_data$responses,
								  type = type,
								  age = age,
								  .width = .width
	) |>
		filter(item_dominance == "L1") |>
		summarise(across(c(.sum, .n), sum), .by = "item")
	
	familiarity <- familiarity |>
		mutate(
			familiarity = prop_adj(.sum, .n),
			familiarity_se = prop_adj_se(.sum, .n)
		) |>
		select(item, starts_with("familiarity")) |>
		rename(word = item) |>
		mutate(test_language = case_when(
			grepl("eng_", word) ~ "English",
			grepl("cat_", word) ~ "Catalan",
			grepl("spa_", word) ~ "Spanish"
		)) |>
		relocate(test_language, .before = word)
	
	return(familiarity)
}

#' Get frequencies from CHILDES
get_childes_corpora <- function(token, languages = c("eng")) {
	# if CHILDES exists, load
	childes.path <- file.path("data-raw", "stimuli", "childes.csv")
	if (file.exists(childes.path)) {
		childes <- readr::read_csv(childes.path, show_col_types = FALSE)
		cli_alert_success("Previous versions of childes loaded")
		return(childes)
	}
	
	# absolute frequency (raw counts)
	childes <- childesr::get_tokens(
		role = "target_child",
		token = token,
		language = languages
	) |>
		mutate(gloss = tolower(gloss)) |>
		filter(grepl(paste(languages, collapse = "|"), language)) |>
		count(gloss, language) |>
		mutate(language = strsplit(language, " ")) |>
		unnest(language) |>
		summarise(
			freq_counts = sum(n),
			.by = c(language, gloss)
		) |>
		filter(freq_counts > 0)
	
	arrow::write_csv_arrow(childes, childes.path)
	
	return(childes)
}

#' Compute lexical frequencies from CHILDES corpora
# WARNING: especial characters are not being handled very well
get_frequency_childes <- function(childes,
								  token, # word(s) form to look up, e.g. c("table", "mesa")
								  languages = c("eng"), # languages in which to look up the word form
								  ... # passed to childesr::get_speaker_statistics)
) {
	suppressMessages({
		# get total number of tokens in each language
		total_counts <- childesr::get_speaker_statistics() |>
			filter(grepl(paste(languages, collapse = "|"), language)) |>
			summarise(num_tokens = sum(num_tokens), .by = language) |>
			mutate(language = strsplit(language, " ")) |>
			unnest(cols = language) |>
			summarise(n = sum(num_tokens, na.rm = TRUE), .by = language)
		
		# relative frequency (counts per million)
		frequencies <- childes |>
			left_join(total_counts, by = "language") |>
			mutate(
				freq_per_million = freq_counts / n * 1e6,
				freq_zipf = log10(freq_per_million) + 3
			) |>
			rename(
				childes_lemma = gloss,
				freq = freq_zipf
			) |>
			select(childes_lemma, freq)
	})
	
	return(frequencies)
}

#' Get stimuli data (join everything)
get_stimuli <- function(trials, words, frequencies, durations, impute = FALSE) {
	d_w <- select(words, -role)
	
	stim <- trials |>
		mutate(audio = stringi::stri_trans_general(
			str = trials$audio,
			id = "Latin-ASCII"
		)) |>
		pivot_longer(c(prime, target, distractor),
					 names_to = "role",
					 values_to = "stimulus"
		) |>
		left_join(d_w, by = join_by(test_language, stimulus)) |>
		left_join(frequencies, by = join_by(childes_lemma)) |>
		select(-c(childes_lemma, wordbank_lemma)) |>
		mutate(nphon = map_int(strsplit(xsampa, ""), length)) |>
		rename(vocab_item = item, vocab_item_supp = item_supp) |>
		pivot_wider(
			id_cols = c(
				trial, location, test_language, version, list,
				audio, target_location, trial_type
			),
			names_from = role,
			values_from = c(
				xsampa, xsampa_t, freq, stimulus, nphon,
				vocab_item, vocab_item_supp
			)
		) |>
		left_join(durations, by = join_by(test_language, version, audio))
	
	if (impute) {
		stim <- stim |>
			mice::mice(printFlag = FALSE) |> # predictive mean matching
			mice::complete() |> # get complete data set
			as_tibble()
	}
	
	# select and reorder relevant variables
	out <- stim |>
		rename_with(\(x) gsub("stimulus_", "", x)) |>
		mutate(
			lv_pp = stringdist::stringsim(xsampa_prime, xsampa_t_prime),
			lv_pt = stringdist::stringsim(xsampa_prime, xsampa_target)
		) |>
		rowwise() |>
		mutate(
			xsampa = list(unlist(across(matches("xsampa")))),
			nphon = list(unlist(across(matches("nphon")))),
			freq = list(unlist(across(matches("freq")))),
			vocab_item = list(unlist(across(c(
				vocab_item_prime,
				vocab_item_target,
				vocab_item_distractor
			)))),
			vocab_item_supp = list(unlist(across(matches("vocab_item_supp"))))
		) |>
		ungroup() |>
		select(
			trial, test_language, version, list, trial_type,
			prime, target, distractor, audio, target_location,
			duration, nphon, freq, xsampa, lv_pp, lv_pt, vocab_item, vocab_item_supp
		) |>
		mutate(across(c(trial, list), as.integer))
	
	# test_stimuli(out)
	
	return(out)
}
