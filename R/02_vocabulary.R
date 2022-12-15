# vocabulary size
get_vocabulary <- function(participants, # participants dataset (get_participants output)
						   update = FALSE, # update vocabulary data?
						   type = "understands", # vocabulary type (understands or produces)
						   bvq_data
){
	suppressMessages({
		
		participants <- select(participants, id, age_group, lp)
		
		# vocabulary sizes BVQ database and Cognate Priming
		to_impute <- bvq_data$vocabulary %>%
			mutate(age_group = as.factor(case_when(between(age, 19, 24) ~ 21,
												   between(age, 24, 28) ~ 25,
												   between(age, 28, 34) ~ 30)),
				   age_group = paste0(age_group, " months")) %>%
			filter(type=="understands") %>% 
			right_join(select(bvq_data$logs, id, age_group)) %>% 
			filter(age_group != "NA months") %>% 
			select(id, age_group,
				   vocab_size_total = vocab_prop_total,
				   vocab_size_l1 = vocab_prop_dominance_l1,
				   vocab_size_conceptual = vocab_prop_conceptual) %>% 
			full_join(select(participants, -lp)) %>% 
			mutate(is_imputed = is.na(vocab_size_l1) | is.na(vocab_size_total) | is.na(vocab_size_conceptual)) %>% 
			relocate(id, age_group, is_imputed)
		
		# multiple imputation
		imputed <- to_impute %>%
			filter(age_group != "Other") %>% 
			split(.$age_group) %>% 
			map_dfr(function(x){
				# index of missing observations to impute
				x <- select(x, -age_group)
				impute_index <- is.na(x)
				impute_index[, 1:2] <- FALSE

				x %>% 
					mice(m = 5, where = impute_index, method = "pmm", seed = 888) %>% 
					complete() %>% 
					as_tibble()
			}, .id = "age_group")
		
		# merge both datasets ----
		vocabulary <- left_join(participants, imputed) %>% 
			mutate(age_group = factor(age_group, levels = paste0(c(21, 25, 30), " months")),
				   lp = factor(lp, levels = c("Monolingual", "Bilingual")))
	})
	
	return(vocabulary)
}


