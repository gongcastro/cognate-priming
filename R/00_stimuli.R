# get familiarity ----
get_familiarity <- function(
		multilex_data,
		tokens, # words to retrieve familiarity for
		type = "understands", # understands or produces
		update = TRUE, # should vocabulary data be updated
		age = c(17, 19), # age range for familiarity norms (min-max)
		.width = 0.95 # confidence level of the estimates
){
	
	familiarity <- ml_norms(
		multilex_data$participants,
		multilex_data$responses, 
		type = type,
		age = age,
		.width = .width
	) %>% 
		filter(item_dominance=="L1") %>% 
		group_by(item) %>% 
		summarise(
			yes = sum(yes, na.rm = TRUE),
			n = sum(n, na.rm = TRUE),
			.groups = "drop"
		) 
	
	familiarity <- familiarity %>% 
		mutate(
			familiarity = prop_adj(yes, n),
			familiarity_se = prop_adj_se(yes, n)
		) %>% 
		select(item, starts_with("familiarity")) %>% 
		rename(word = item) %>% 
		mutate(
			test_language = case_when(
				grepl("eng_", word) ~ "English",
				grepl("cat_", word) ~ "Catalan",
				grepl("spa_", word) ~ "Spanish"
			)
		) %>% 
		relocate(test_language, .before = word)
	
	return(familiarity)
}


# get frequencies from CHILDES ----
get_childes_corpora <- function(
		token,
		languages = c("cat", "spa")
){
	
	if (file.exists("data/stimuli/childes.csv")){
		
		childes <- read_csv_arrow("data/stimuli/childes.csv")
		
		ui_done("Previous versions of childes loaded")
		
	} else {
		
		# absolute frequency (raw counts)
		childes <- get_tokens(
			role = "target_child", 
			token =  token,
			language = languages
		) %>%
			mutate(gloss = str_to_lower(gloss)) %>%
			filter(str_detect(language, paste(languages, collapse = "|"))) %>%
			count(gloss, language) %>%
			mutate(language = str_split(language, " ")) %>%
			unnest(language) %>%
			group_by(language, gloss) %>%
			summarise(
				freq_counts = sum(n), 
				.groups = "drop"
			) %>%
			filter(freq_counts > 0)
		
		write_csv_arrow(childes, "data/stimuli/childes.csv")
		
	}
	
	return(childes)
}

# compute lexical frequencies from CHILDES corpora ----
# WARNING: especial characters are not being handled very well
get_frequency_childes <- function(
		childes,
		token, # word(s) form to look up, e.g. c("table", "mesa")
		languages = c("cat", "spa"), # languages in which to look up the word form 
		... # other arguments (see ?childesr::get_speaker_statistics)
){
	
	if (file.exists("data/stimuli/frequency_childes.csv")){
		
		frequency <- read_csv_arrow("data/stimuli/frequency_childes.csv")
		ui_done("Previous versions of frequency_childes loaded")
		
	} else {
		
		suppressMessages({
			
			# get total number of tokens in each language
			total_counts <- get_speaker_statistics() %>%
				filter(str_detect(language, paste(languages, collapse = "|"))) %>%
				group_by(language) %>%
				summarise(
					num_tokens = sum(num_tokens), 
					.groups = "drop"
				) %>%
				mutate(language = str_split(language, " ")) %>%
				unnest(cols = language) %>%
				group_by(language) %>%
				summarise(
					n = sum(num_tokens, na.rm = TRUE),
					.groups = "drop"
				)
			
			# relative frequency (counts per million)
			frequency <- childes %>%
				left_join(total_counts, by = "language") %>%
				mutate(
					freq_per_million = freq_counts/n*1e6,
					freq_zipf = log10(freq_per_million)+3,
					language = str_replace_all(
						language, c(
							"eng" = "English", 
							"spa" = "Spanish",
							"cat" = "Catalan"
						)
					)
				) %>%
				rename(
					word = gloss,
					test_language = language, 
					frequency = freq_zipf
				) %>%
				select(word, test_language, frequency)
			
		})
		
		write_csv_arrow(frequency, "data/stimuli/frequency_childes.csv")
	}
	
	return(frequency)
}

# get SUBTLEX lexical frequencies ----
get_frequency_subtlex <- function(
		token,
		languages = c("spa", "cat")
){
	suppressMessages({
		
		subtlex_eng <- NULL
		subtlex_cat <- NULL
		subtlex_esp <- NULL
		
		if ("eng" %in% languages){
			
			subtlex_eng <- fread(
				"http://crr.ugent.be/papers/SUBTLEX-UK.txt",
				verbose = FALSE, showProgress = FALSE
			) %>%
				as_tibble() %>% 
				clean_names() %>% 
				rename(word = spelling, frequency = log_freq_zipf) %>% 
				select(word, frequency)
		}
		
		if ("spa" %in% languages){
			
			tf <- tempfile(fileext = "zip")
			
			GET("http://crr.ugent.be/papers/SUBTLEX-ESP.zip", write_disk(tf))
			
			subtlex_spa <- read_excel(unzip(tf)) %>% 
				clean_names() %>%
				select(starts_with("word_"), starts_with("log_")) %>% 
				rename_at(vars(starts_with("log")), ~gsub("log_freq", "frequency", .)) %>% 
				drop_na() 
			
			colnames(subtlex_spa) <- paste(rep(c("word", "frequency"), each = 3), 1:3, sep = "_")
			
			subtlex_spa <- pivot_longer(
				subtlex_spa,
				everything(),
				names_to = c(".value", "set"),
				names_pattern =  "(.)_(.)"
			) %>% 
				select(d, y)
			
			colnames(subtlex_spa) <- c("word", "frequency")
			
			file.remove("SUBTLEX-ESP.xlsx")
		}
		
		if("cat" %in% languages){
			
			tf <- tempfile(fileext = "xlsx")
			
			GET("https://psico.fcep.urv.cat/projectes/gip/papers/SUBTLEX-CAT.xlsx", write_disk(tf))
			
			subtlex_cat <- read_excel(tf) %>% 
				clean_names() %>%
				rename(word = words, frequency = zipf) %>% 
				select(word, frequency)
		}
		
		frequency <- list(English = subtlex_eng, Spanish = subtlex_spa, Catalan = subtlex_cat) %>% 
			bind_rows(.id = "test_language") %>% 
			select(word, test_language, frequency) %>% 
			filter(word %in% token)
	})
	
	return(frequency)
}



# stimuli ----
get_stimuli <- function(
		trials, # trials dataset
		familiarity,
		frequency_childes,
		frequency_subtlex,
		animacy,
		semantic_category,
		impute = TRUE # impute missing data?
){
	suppressMessages({
		
		
		# to avoid issues with other column names
		frequency_childes <- rename(frequency_childes, frequency_childes = frequency) 
		frequency_subtlex <- rename(frequency_subtlex, frequency_subtlex = frequency) 
		
		# merge data ----
		# join all datasets (not very elegant, but does the trick
		# prime and target data are joined separately to avoid duplicating entries)
		stimuli_raw <- trials %>% 
			left_join(
				semantic_category,
				by = c("target_cdi" = "word", "test_language")
			) %>% 
			rename(category_prime = category) %>% 
			left_join(
				semantic_category,
				by = c("prime_cdi" = "word", "test_language")
			) %>% 
			rename(category_target = category) %>% 
			left_join(
				frequency_subtlex,
				by = c("prime" = "word", "test_language")
			) %>% 
			rename(frequency_prime_subtlex = frequency_subtlex) %>% 
			left_join(
				frequency_subtlex, 
				by = c("target" = "word", "test_language")
			) %>% 
			rename(frequency_target_subtlex = frequency_subtlex) %>% 
			left_join(
				frequency_childes, 
				by = c("prime" = "word", "test_language")
			) %>% 
			rename(frequency_prime_childes = frequency_childes) %>% 
			left_join(
				frequency_childes,
				by = c("target" = "word", "test_language")
			) %>% 
			rename(frequency_target_childes = frequency_childes) %>% 
			left_join(
				animacy,
				by = c("prime" = "object", "test_language")
			) %>% 
			rename(is_animate_prime = is_animate) %>% 
			left_join(
				animacy,
				by = c("target" = "object", "test_language")
			) %>% 
			rename(is_animate_target = is_animate) %>% 
			left_join(
				familiarity,
				c("prime_cdi" = "word", "test_language")) %>% 
			rename_at(
				vars(starts_with("familiarity")), 
				function(x) paste0(x, "_prime")
			) %>% 
			left_join(
				familiarity,
				c("target_cdi" = "word", "test_language")
			) %>% 
			rename_at(
				vars(starts_with("familiarity") & !ends_with("_prime")), 
				function(x) paste0(x, "_target")
			) %>% 
			rename_all(function(x) str_replace(x, "prime_target", "prime")) %>% 
			filter(location=="Barcelona")
		
		# impute data
		if (impute){ # defined in arguments
			
			stimuli_imputed <- stimuli_raw %>% 
				mice(printFlag = FALSE) %>% # predictive mean matching is used by default
				complete() %>% # get complete dataset
				as_tibble()
			
		}
		
		stimuli <- stimuli_imputed %>% 
			select(
				trial, test_language, version, list, trial_type,
				prime, target, distractor, audio, target_location,
				prime_cdi, target_cdi, distractor_cdi,
				valid_trial, 
				familiarity_prime, familiarity_target,
				familiarity_se_prime, familiarity_se_target,
				frequency_prime_childes, frequency_target_childes,
				category_prime, category_target,
				is_animate_prime, is_animate_target,
				frequency_prime_subtlex, frequency_target_subtlex,
				is_animate_prime, is_animate_target
			) %>% 
			mutate(
				trial = as.integer(trial),
				is_animate_prime = as.logical(is_animate_prime),
				is_animate_target = as.logical(is_animate_target),
				list = as.integer(list)
			)
	})
	
	return(stimuli)
}
