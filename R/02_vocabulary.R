# vocabulary size

# set up ----

get_vocabulary <- function(
	participants, # participants dataset (get_participants output)
	update = FALSE, # update vocabulary data?
	type = "understands", # vocabulary type (understands or produces)
	impute = TRUE, # impute missing data?
	vocabulary_oxf, # Oxford vocabulary data
	multilex_data
){
	suppressMessages({
		
		cdi_replacements <- c(
			"boots" = "boot", "boot(s)" = "boot", "fire engine" = "fireengine",
			"teddy bear" = "teddy", "bunny / rabbit" = "bunny",
			"lorry / truck" = "truck", "bicycle / bike" = "bike"
		)
		
		# Barcelona vocabulary ----
		# norms (by item)
		vocab_items_bcn <- multilex_data$responses %>% 
			rename(participant = id_exp) %>% 
			mutate(
				understands = response > 1,
				age_group =  as.factor(
					case_when(
						between(age, 19, 24) ~ 21,
						between(age, 24, 28) ~ 25,
						between(age, 28, 34) ~ 30
					)),
				age_group = paste0(age_group, " months")
			) %>%
			filter(study=="CognatePriming", age_group != "NA months", understands) %>%
			select(participant, time, age_group, item, response) %>%
			group_by(participant, age_group) %>%
			summarise(vocab_words = list(unique(item)), .groups = "drop")
		
		# norms (by participant)
		vocab_size_bcn <- multilex_data$vocabulary %>%
			rename(participant = id_exp) %>% 
			right_join(select(multilex_data$logs, participant, time)) %>% 
			mutate(
				age_group =  as.factor(
					case_when(
						between(age, 19, 24) ~ 21,
						between(age, 24, 28) ~ 25,
						between(age, 28, 34) ~ 30
					)),
				age_group = paste0(age_group, " months")
			) %>%
			filter(
				age_group != "NA months",
				type=="understands"
			) %>% 
			select(
				participant, age_group,
				vocab_size_total = vocab_prop_total,
				vocab_size_l1 = vocab_prop_dominance_l1,
				vocab_size_conceptual = vocab_prop_conceptual
			)
		
		# impute vocab size
		vocab_size_imputed_bcn <- full_join(
			select(participants, participant, age_group, lp),
			vocab_size_bcn
		) %>% 
			mutate(
				is_imputed = is.na(vocab_size_l1) |
					is.na(vocab_size_total) |
					is.na(vocab_size_conceptual)
			)
		
		if (impute){
			vocab_size_bcn <- vocab_size_bcn %>% 
				mice(m = 5, method = "pmm", seed = 888, printFlag = FALSE) %>% 
				complete() %>% 
				as_tibble()
		}
		
		# item norms (by item in whole database)
		vocab_norms_item_bcn <- multilex_data$responses %>%
			rename(participant = id_exp) %>% 
			left_join(multilex_data$logs) %>% 
			filter(language==dominance) %>% 
			mutate(
				understands = response > 1,
				age_group =  as.factor(
					case_when(
						between(age, 19, 24) ~ 21,
						between(age, 24, 28) ~ 25,
						between(age, 28, 34) ~ 30
					)),
				age_group = paste0(age_group, " months")
			) %>%
			filter(age_group != "NA months") %>% 
			group_by(item, age_group, lp) %>% 
			summarise(
				prop = mean(understands, na.rm = TRUE),
				n = n(),
				.groups = "drop") %>% 
			filter(prop > 0.5) %>% 
			group_by(age_group, lp) %>%
			summarise(vocab_words = list(unique(item)), .groups = "drop")
		
		# impute barcelona data
		vocab_bcn <- participants %>% 
			filter(location=="Barcelona") %>% 
			select(participant, age_group) %>% 
			left_join(vocab_size_imputed_bcn) %>% 
			left_join(vocab_items_bcn) %>% 
			mutate(is_imputed_vocab_words = map_lgl(vocab_words, is.null)) %>% 
			group_split(is_imputed_vocab_words) 
		
		vocab_bcn[[2]] <- vocab_bcn[[2]] %>% 
			select(-vocab_words) %>% 
			left_join(vocab_norms_item_bcn)
		
		vocab_bcn <- bind_rows(vocab_bcn) %>% 
			select(-c(lp, is_imputed_vocab_words))
		
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
			select(-category) %>% 
			mutate(item = str_replace_all(item, cdi_replacements))
		
		vocab_size_oxf <- vocab_raw_oxf %>%
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
			)
		
		vocab_oxf <- vocab_raw_oxf %>%
			filter(understands) %>%
			group_by(participant, age_group) %>%
			summarise(
				vocab_words = list(unique(item)), # list of understood words
				.groups = "drop"
			) %>%
			right_join(vocab_size_oxf) %>% 
			mutate(is_imputed = FALSE) %>% 
			select(-n)
		
		# merge datasets ----
		vocab <- bind_rows(vocab_bcn, vocab_oxf)
	})
	return(vocab)
}