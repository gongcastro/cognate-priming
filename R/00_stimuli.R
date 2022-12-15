# get familiarity
get_familiarity <- function(
		bvq_data,
		tokens, # words to retrieve familiarity for
		type = "understands", # understands or produces
		update = TRUE, # should vocabulary data be updated
		age = c(17, 19), # age range for familiarity norms (min-max)
		.width = 0.95 # confidence level of the estimates
){
	
	familiarity <- bvq_norms(bvq_data$participants,
							 bvq_data$responses, 
							 type = type,
							 age = age,
							 .width = .width) %>% 
		filter(item_dominance=="L1") %>% 
		group_by(item) %>% 
		summarise(yes = sum(yes, na.rm = TRUE),
				  n = sum(n, na.rm = TRUE),
				  .groups = "drop") 
	
	familiarity <- familiarity %>% 
		mutate(familiarity = prop_adj(yes, n),
			   familiarity_se = prop_adj_se(yes, n)) %>% 
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


# get frequencies from CHILDES
get_childes_corpora <- function(token, languages = c("cat", "spa")) {
	
	if (file.exists("data/stimuli/childes.csv")){
		childes <- read_csv_arrow("data/stimuli/childes.csv")
		cli::cli_alert_success("Previous versions of childes loaded")
	} else {
		
		# absolute frequency (raw counts)
		childes <- get_tokens(role = "target_child", 
							  token =  token,
							  language = languages) %>%
			mutate(gloss = str_to_lower(gloss)) %>%
			filter(str_detect(language, paste(languages, collapse = "|"))) %>%
			count(gloss, language) %>%
			mutate(language = str_split(language, " ")) %>%
			unnest(language) %>%
			group_by(language, gloss) %>%
			summarise(freq_counts = sum(n), .groups = "drop") %>%
			filter(freq_counts > 0)
		
		write_csv_arrow(childes, "data/stimuli/childes.csv")
		
	}
	
	return(childes)
}

# compute lexical frequencies from CHILDES corpora ----
# WARNING: especial characters are not being handled very well
get_frequency_childes <- function(childes,
								  token, # word(s) form to look up, e.g. c("table", "mesa")
								  languages = c("cat", "spa"), # languages in which to look up the word form 
								  ... # passed to childesr::get_speaker_statistics)
){
	
	suppressMessages({
		# get total number of tokens in each language
		total_counts <- get_speaker_statistics() %>%
			filter(str_detect(language, paste(languages, collapse = "|"))) %>%
			group_by(language) %>%
			summarise(num_tokens = sum(num_tokens), .groups = "drop") %>%
			mutate(language = str_split(language, " ")) %>%
			unnest(cols = language) %>%
			group_by(language) %>%
			summarise(n = sum(num_tokens, na.rm = TRUE), .groups = "drop")
		
		# relative frequency (counts per million)
		frequencies <- childes %>%
			left_join(total_counts, by = "language") %>%
			mutate(freq_per_million = freq_counts/n*1e6,
				   freq_zipf = log10(freq_per_million)+3,
				   language = str_replace_all(language,
				   						   c("eng" = "English", 
				   						     "spa" = "Spanish",
				   						     "cat" = "Catalan"))) %>%
			rename(word = gloss, test_language = language, freq = freq_zipf) %>%
			select(word, test_language, freq)
	})
	
	return(frequencies)
}




# stimuli
get_stimuli <- function(trials, # trials dataset
						familiarity,
						frequencies,
						animacy,
						semantic_category,
						impute = TRUE){
	suppressMessages({
		
		# to avoid issues with other column names
		
		# merge data ----
		# join all datasets (not very elegant, but does the trick
		# prime and target data are joined separately to avoid duplicating entries)
		stimuli_raw <- trials %>% 
			left_join(semantic_category, by = c("target_cdi" = "word", "test_language")) %>% 
			rename(semantic_category_prime = semantic_category) %>% 
			left_join(semantic_category, by = c("prime_cdi" = "word", "test_language")) %>% 
			rename(semantic_category_target = semantic_category) %>% 
			left_join(frequencies, by = c("prime" = "word", "test_language")) %>% 
			rename(freq_prime = freq) %>%
			left_join(frequencies, by = c("target" = "word", "test_language")) %>% 
			rename(freq_target = freq) %>% 
			left_join(animacy, by = c("prime" = "object", "test_language")) %>% 
			rename(is_animate_prime = is_animate) %>% 
			left_join(animacy, by = c("target" = "object", "test_language")) %>% 
			rename(is_animate_target = is_animate) %>% 
			left_join(familiarity, c("prime_cdi" = "word", "test_language")) %>% 
			rename_at(vars(starts_with("familiarity")), function(x) paste0(x, "_prime")) %>% 
			left_join(familiarity, c("target_cdi" = "word", "test_language")
			) %>% 
			rename_with(~paste0(., "_target"), c(starts_with("familiarity") & !ends_with("_prime"))) %>% 
			rename_with(~str_replace(., "prime_target", "prime"), everything()) %>% 
			filter(location=="Barcelona")
		
		# impute data
		if (impute){ # defined in arguments
			stimuli_imputed <- stimuli_raw %>% 
				mice(printFlag = FALSE) %>% # predictive mean matching is used by default
				complete() %>% # get complete dataset
				as_tibble()
		}
		
		stimuli <- stimuli_imputed %>% 
			select(trial, test_language, version, list, trial_type,
				   prime, target, distractor, audio, target_location,
				   prime_cdi, target_cdi, distractor_cdi,
				   valid_trial, 
				   familiarity_prime, familiarity_target,
				   familiarity_se_prime, familiarity_se_target,
				   freq_prime, freq_target,
				   semantic_category_prime, semantic_category_target,
				   is_animate_prime, is_animate_target,
				   is_animate_prime, is_animate_target) %>% 
			mutate(across(c(trial, list), as.integer),
				   across(starts_with("is_animate"), as.logical))
	})
	
	return(stimuli)
}
