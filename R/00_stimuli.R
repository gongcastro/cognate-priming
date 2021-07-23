# stimuli

get_stimuli <- function(
	trials, # trials dataset
	familiarity_age = c(17, 19), # age range for familiarity norms
	familiarity_type = c("understands"), # vocabulary type (understands or produces)
	impute = TRUE, # impute missing data?
	update = FALSE, # update vocabulary data?
	conf = 0.95, # confidence level
	multilex_data,
	oxford_data,
	animacy_data
){
	suppressMessages({
		
		get_credentials()
		
		# familiarity norms ----
		familiarity <- get_familiarity(
			tokens =  distinct(trials, prime_cdi, target_cdi) %>% unlist() %>% unique(),
			oxford_data = oxford_data,
			type = familiarity_type,
			update = update,
			multilex_data = multilex_data
		)
		
		
		# lexical frequency norms (CHILDES) ----
		frequency_childes <- get_frequency_childes(
			token = distinct(trials, prime, target) %>% unlist() %>% unique()
		) %>% 
			rename(frequency_childes = frequency) 
		
		
		# lexical frequency norms (SUBTLEX) ----
		frequency_subtlex <- get_frequency_subtlex(
			token = distinct(trials, prime, target) %>% unlist() %>% unique()
		) %>% 
			rename(frequency_subtlex = frequency) 
		
		# semantic category ----
		semantic_category <- select(multilex_data$pool, word = item, language, category) %>%
			rename(test_language = language)
		
		# animacy ----
		animacy <- animacy_data %>% 
			as_tibble() %>% 
			mutate(is_animate = as.logical(is_animate)) %>% 
			as_tibble()
		
		# merge data ----
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
		if (impute){
			stimuli <- stimuli %>% 
				mice(printFlag = FALSE) %>% 
				complete() %>% 
				as_tibble()
			
		}
	})
	return(stimuli)
}
