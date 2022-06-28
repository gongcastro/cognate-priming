# vocabulary size

# set up ----

get_vocabulary <- function(
		participants, # participants dataset (get_participants output)
		update = FALSE, # update vocabulary data?
		type = "understands", # vocabulary type (understands or produces)
		multilex_data
){
	suppressMessages({
		
		participants <- select(participants, participant, age_group, lp)
		
		# vocabulary sizes multilex database and cognatepriming
		to_impute <- multilex_data$vocabulary %>%
			mutate(
				age_group = as.factor(
					case_when(
						between(age, 19, 24) ~ 21,
						between(age, 24, 28) ~ 25,
						between(age, 28, 34) ~ 30
					)
				),
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
			full_join(select(participants, -lp)) %>% 
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
		
		# merge both datasets ----
		vocabulary <- left_join(participants, imputed) %>% 
			mutate(
				age_group = factor(age_group, levels = paste0(c(21, 25, 30), " months")),
				lp = factor(lp, levels = c("Monolingual", "Bilingual"))
			)
	})
	
	# save output
	write_csv_arrow(vocabulary, here("data", "vocabulary.csv"))
	
	return(vocabulary)
}


