# vocabulary size

# set up ----

get_vocabulary <- function(
	participants, # participants dataset (get_participants output)
	update = FALSE, # update vocabulary data?
	type = "understands", # vocabulary type (understands or produces)
	vocabulary_oxf, # Oxford vocabulary data
	multilex_data
){
	suppressMessages({
		
		
		
		# Barcelona vocabulary ----
		participants_bcn <- participants %>% 
			filter(location=="Barcelona") %>% 
			select(participant, age_group, lp)
		
		# vocabulary sizes multilex database and cognatepriming
		to_impute <- multilex_data$vocabulary %>%
			mutate(
				age_group = as.factor(
					case_when(
						between(age, 19, 24) ~ 21,
						between(age, 24, 28) ~ 25,
						between(age, 28, 34) ~ 30
					)),
				age_group = paste0(age_group, " months")
			) %>%
			filter(type=="understands") %>% 
			rename(participant = id_exp) %>% 
			right_join(select(multilex_data$logs, participant, age_group)) %>% 
			filter(age_group != "NA months") %>% 
			select(
				participant, age_group,
				vocab_size_total = vocab_prop_total,
				vocab_size_l1 = vocab_prop_dominance_l1,
				vocab_size_conceptual = vocab_prop_conceptual
			) %>% 
			full_join(select(participants_bcn, -lp)) %>% 
			mutate(is_imputed = is.na(vocab_size_l1) | is.na(vocab_size_total) | is.na(vocab_size_conceptual))
		
		# index of missing observations to impute
		impute_index <- is.na(to_impute)
		impute_index[, 1:2] <- FALSE
		
		# multiple imputation
		imputed <- mice( 
			to_impute,
			m = 5, 
			where = impute_index,
			# predictorMatrix = pred_matrix,
			method = "pmm",
			seed = 888,
			printFlag = FALSE
		) %>% 
			complete() %>% 
			as_tibble()
		
		vocabulary_bcn <- left_join(participants_bcn, imputed)
		
		# Oxford vocabulary ----
		vocab_raw_oxf <- vocabulary_oxf %>%
			mutate_at(vars(-c(item, version)), as.integer) %>%
			pivot_longer(-c(item, version), names_to = "participant", values_to = "response") %>%
			#separate(item, c("category", "item"), sep = " - ", fill = "left") %>% 
			mutate(
				understands = response %in% c(1, 2),
				says = response %in% 2,
				# define age group
				age_group = case_when(
					substr(participant, start = 1, stop = 2) %in% c(21) ~ 21,
					substr(participant, start = 1, stop = 2) %in% c(24, 25) ~ 25,
					substr(participant, start = 1, stop = 2) %in% c(27, 30) ~ 30
				),
				age_group = as.factor(paste0(age_group, " months"))
			) %>%
			separate(item, c("category", "item"), sep = " - ", fill = "left") %>% 
			drop_na(response) %>%
			arrange(participant, version) %>% 
			select(-category) 

		vocabulary_oxf <- vocab_raw_oxf %>%
			group_by(participant, age_group) %>%
			summarise(
				vocab_size_total = mean(understands, na.rm = TRUE), # proportion of understood words
				n = n(),
				.groups = "drop"
			) %>% 
			mutate(
				vocab_size_total = ifelse(n < 100, NA, vocab_size_total), # remove incomplete vocabulary responses
				vocab_size_l1 = vocab_size_total,
				vocab_size_conceptual = vocab_size_total
			) %>% 
			mutate(is_imputed = FALSE) %>% 
			select(-n) %>% 
			left_join(select(participants, participant, age_group, lp))
		
		# merge both datasets ----
		vocabulary <- bind_rows(vocabulary_bcn, vocabulary_oxf)
	})
	
	# save output
	saveRDS(vocabulary, here("results", "vocabulary_barcelona.rds"))
	
	return(vocabulary)
}


