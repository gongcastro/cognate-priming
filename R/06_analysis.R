# analysis

# fit main model ----
fit_main_model <- function(
	data
){
	fit <- lmer(
		elog ~ age_group + vocab_size_l1_center*trial_type*lp*(ot1+ot2+ot3) +
			(1+ot1+ot2+ot3+trial_type+age_group | participant),
		control = lmerControl(optimizer = "bobyqa"),
		data = data
	)
	return(fit)
}

# fit models with only 21mo monolingual data ----
fit_21_mon_model <- function(
	data
){
	data <- filter(data, age_group=="21 months", lp=="Monolingual")
	fit <- lmer(
		elog ~  vocab_size_l1_center*trial_type*(ot1+ot2+ot3) +
			(1+ot1+ot2+ot3+trial_type | participant),
		control = lmerControl(optimizer = "bobyqa"),
		data = data
	)
	return(fit)
}

# fit model with a median split in vocabulary
fit_21_mon_vocab_split_model <- function(
	data,
	vocabulary
){
	data <- data %>% 
		filter(age_group=="21 months", lp=="Monolingual") %>% 
		mutate(vocab_cat = ifelse(
			vocab_size_l1_center > median(distinct(data, participant, vocab_size_l1_center)$vocab_size_l1_center, na.rm = TRUE), 
			"Above median", 
			"Below median"
		)) %>% 
		mutate_at(vars(vocab_cat, location), as.factor)
	
	contrasts(data$vocab_cat) <- c(0.5, -0.5)
	contrasts(data$location) <- c(0.5, -0.5)
	
	fit <- lmer(
		elog ~ trial_type*vocab_cat*(ot1+ot2+ot3) +
			(1+ot1+ot2 | participant),
		control = lmerControl(optimizer = "bobyqa"),
		data = data
	)
	return(fit)
}

# fit model selecting participants with balanced vocabulary
fit_21_mon_vocab_balanced_model <- function(
	data,
	participants,
	vocabulary
){
	# 21mo monolinguals only
	vocab_max_min <- participants %>%
		left_join(vocabulary) %>% 
		drop_na(vocab_size_l1) %>% 
		filter(age_group=="21 months", lp=="Monolingual") %>% 
		arrange(location, desc(vocab_size_l1)) %>% 
		group_by(location) %>% 
		mutate(vocab_index = row_number()) %>% 
		ungroup() %>% 
		filter(
			((location=="Barcelona") & (vocab_index %in% 1:15)) |
				((location=="Oxford")) & (vocab_index %in% seq(
					max(vocab_index[location=="Oxford"])-15,
					max(vocab_index[location=="Oxford"]))
				)
		) 
	
	data <- filter(data, participant %in% vocab_max_min$participant)
	fit <- lmer(
		elog ~ trial_type*(ot1+ot2+ot3) +
			(1+ot1+ot2+ot3 | participant),
		control = lmerControl(optimizer = "bobyqa"),
		data = data
	)
	return(fit)
}

# fit models with only 21mo monolingual data by location ----
fit_21_mon_location_model <- function(
	data
){
	data <- filter(data, age_group=="21 months", lp=="Monolingual")
	fit <- lmer(
		elog ~ trial_type*location*(ot1+ot2+ot3) + (1+ot1+ot2 | participant),
		control = lmerControl(optimizer = "bobyqa"),
		data = data
	)
	return(fit)
}






