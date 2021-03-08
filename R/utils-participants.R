# utils for participants -------------------------------------------------------

# get sample -------------------------------------------------------------------
sample_get <- function(
	google_email = NULL,
	pilot = FALSE
) {
	gs4_auth(google_email) 	# log into Google
	
	# retrieve participant info
	x <- suppressMessages({
		range_read(ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", sheet = "Participants", na = "") %>%
			drop_na(participant_id, date_test) %>%
			{if (!pilot) filter(., !pilot)}
	}) %>% 
		mutate_if(is.logical, function(x) ifelse(is.na(x), FALSE, x))
	return(x)
}

# summarise sample
sample_summary <- function(sample = NULL, ..., google_email = NULL) {
	
	# if not provided, generate sample
	if (is.null(sample)) {
		if (is.null(google_email)){
			google_email <- readline(prompt = "Google email: ")
			sample <- sample_get(google_email)
		} else {
			sample <- sample_get(google_email)
		}
	}
	
	# get all possible conditions
	sample_conditions <- sample %>%
		expand(...) %>%
		arrange(...)
	
	# summarise sample size by condition
	sample_distribution <- sample %>%
		count(...) %>%
		full_join(sample_conditions) %>%
		replace_na(list(n = 0))
	return(sample_distribution)
	
}



# valid participants -----------------------------------------------------------
get_valid_participants <- function(x, threshold = 2) {
	valid_participants <- x %>%
		filter(valid_trial, valid_other) %>%
		count(participant, trial_type) %>%
		mutate(valid_condition = n >= threshold) %>%
		group_by(participant) %>%
		summarise(
			valid_participant = all(valid_condition),
			.groups = "drop"
		) %>%
		filter(valid_participant) %>%
		pull(participant)
	return(mutate(x, valid_participant = participant %in% valid_participants))
}



