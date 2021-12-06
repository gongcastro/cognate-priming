# stimuli

get_stimuli <- function(
	trials, # trials dataset
	familiarity_age = c(17, 19), # age range for familiarity norms (min-max)
	familiarity_type = c("understands"), # vocabulary type (understands or produces)
	impute = TRUE, # impute missing data?
	update = FALSE, # update vocabulary data? (multilex may take a while to update)
	conf = 0.95, # confidence level for familiarity estimates
	multilex_data, # output of get_multilex()
	oxford_data,
	animacy_data
){
	suppressMessages({
		
		get_credentials()
		
		# familiarity norms ----
		# see utils.R for details on the get_familiarity() function
		familiarity <- get_familiarity(
			# words to compute familiarity norms for (primes and targets)
			tokens =  trials %>% # defined in arguments
				distinct(prime_cdi, target_cdi) %>%
				unlist() %>%
				unique(),
			oxford_data = oxford_data,
			type = familiarity_type, # defined in arguments
			update = update, # defined in arguments
			multilex_data = multilex_data # defined in arguments
		)
		
		
		# lexical frequency norms (CHILDES) ----
		# see utils.R for details on the get_frequency_childes() function
		frequency_childes <- get_frequency_childes(
			# words to compute CHILDES frequency norms for (primes and targets)
			token = trials %>% # defined in arguments
				distinct(prime, target) %>%
				unlist() %>%
				unique()
		) %>% 
			# to avoid issues with other column names
			rename(frequency_childes = frequency) 
		
		
		# lexical frequency norms (SUBTLEX) ----
		# see utils.R for details on the get_frequency_subtlex() function
		frequency_subtlex <- get_frequency_subtlex(
			# words to compute SUBTLEX frequency norms for (primes and targets)
			token = trials %>% # defined in arguments
				distinct(prime, target) %>%
				unlist() %>%
				unique()
		) %>% 
			rename(frequency_subtlex = frequency) 
		
		# semantic category ----
		# only available for Barcelona for now (pending: include Oxford categories)
		semantic_category <- multilex_data$pool %>% # defined in arguments
			select(word = item, language, category) %>%
			rename(test_language = language)
		
		# animacy ----
		animacy <- animacy_data %>% # defined in arguments
			as_tibble() %>% 
			mutate(is_animate = as.logical(is_animate)) %>% 
			as_tibble()
		
		# merge data ----
		# join all datasets (not very elegant, but does the trick
		# prime and target data are joined separately to avoid duplicating entries)
		stimuli <- trials %>% 
			left_join(semantic_category, by = c("target_cdi" = "word", "test_language")) %>% 
			rename(category_prime = category) %>% 
			left_join(semantic_category, by = c("prime_cdi" = "word", "test_language")) %>% 
			rename(category_target = category) %>% 
			left_join(frequency_subtlex, by = c("prime" = "word", "test_language")) %>% 
			rename(frequency_prime_subtlex = frequency_subtlex) %>% 
			left_join(frequency_subtlex, by = c("target" = "word", "test_language")) %>% 
			rename(frequency_target_subtlex = frequency_subtlex) %>% 
			left_join(frequency_childes, by = c("prime" = "word", "test_language")) %>% 
			rename(frequency_prime_childes = frequency_childes) %>% 
			left_join(frequency_childes, by = c("target" = "word", "test_language")) %>% 
			rename(frequency_target_childes = frequency_childes) %>% 
			left_join(animacy, by = c("prime" = "object", "test_language")) %>% 
			rename(is_animate_prime = is_animate) %>% 
			left_join(animacy, by = c("target" = "object", "test_language")) %>% 
			rename(is_animate_target = is_animate) %>% 
			left_join(familiarity, c("prime_cdi" = "word", "test_language")) %>% 
			rename_at(vars(starts_with("familiarity")), 
					  function(x) paste0(x, "_prime")) %>% 
			left_join(familiarity, c("target_cdi" = "word", "test_language")) %>% 
			rename_at(vars(starts_with("familiarity") & !ends_with("_prime")), 
					  function(x) paste0(x, "_target")) %>% 
			rename_all(function(x) str_replace(x, "prime_target", "prime"))
		
		# impute data
		if (impute){ # defined in arguments
			stimuli <- stimuli %>% 
				mice(printFlag = FALSE) %>% # predictive mean matching is used by default
				complete() %>% # get complete dataset
				as_tibble()
			
		}
	})
	return(stimuli)
}