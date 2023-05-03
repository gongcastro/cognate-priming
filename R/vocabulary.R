# vocabulary size
get_vocabulary <- function(participants, # participants dataset (get_participants output)
						   bvq_data
){
	suppressMessages({
		
		# vocabulary sizes BVQ database and Cognate Priming
		
		participants_tmp <- select(participants, id, age_group, lp)
		
		to_impute <- bvq_data$vocabulary %>%
			right_join(select(participants_tmp, id, age_group, lp),
					   by = join_by(id, age_group)) |> 
			rowwise() |> 
			mutate(is_imputed = any(is.na(c_across(total_prop:te_prop)))) %>% 
			ungroup() |> 
			relocate(id, age_group, is_imputed)
		
		impute <- function(x) {
			# index of missing observations to impute
			x <- select(x, id, lp, age_group, ends_with("_prop"))
			impute_index <- is.na(x)
			impute_index[, 1:3] <- FALSE
			
			mice(x, 
				 m = 5,
				 where = impute_index,
				 method = "pmm", 
				 seed = 888) %>% 
				complete() %>% 
				as_tibble()
		}
		
	# multiple imputation
		imputed <- to_impute %>%
			filter(age_group != "Other") %>% 
			impute()
		
		# merge both datasets ----
		vocabulary <- left_join(participants_tmp, imputed,
								by = join_by(id, age_group, lp)) %>% 
			left_join(select(to_impute, id, age_group, is_imputed),
					  by = join_by(id, age_group)) |> 
			mutate(age_group = factor(age_group,
									  levels = paste0(c(21, 25, 30), " months")),
				   lp = factor(lp, levels = c("Monolingual", "Bilingual")))
	})
	
	return(vocabulary)
}


