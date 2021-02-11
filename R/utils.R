#### utils: helper functions ---------------------------------------------------

# evaluate if x is NOT an element of y
"%!in%" <- function(x, y){!(x %in% y)}

# replace NaNs with NAs
nan_to_na <- function(x) ifelse(is.nan(x), NA_real_, x)
	
# get sample
sample_get <- function(
	google_email = NULL,
	pilot = FALSE
) {
	
	# log into Google
	gs4_auth(google_email)
	
	# retrieve participant info
	d <- suppressMessages({
		range_read(
			ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE",
			sheet = "Participants",
			na = ""
		) %>%
			drop_na(participant_id, date_test) %>%
			{if (!pilot) filter(., !pilot)}
	})
	# return output
	return(d)
}

# summarise sample
sample_summary <- function(
	sample = NULL,
	...,
	google_email = NULL
) {
	
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
	
	# return outcome
	return(sample_distribution)
	
}

# replace column names
rename_cols <- function(x){
	str_replace_all(
		x,
		c(
			"system_time_stamp" = "time",
			"l_x" = "l_1",
			"l_y" = "l_2",
			"r_x" = "r_1",
			"r_y" = "r_2",
			"l_user_coord_z" = "l_origin_user_coord_3",
			"r_user_coord_z" = "r_origin_user_coord_3"
		)
	)
}

# evaluate of gaze is in AOI
gaze_in_aoi <- function(
	x,
	y,
	coords = c(710, 1210, 290, 790)
){
	g <- (x >= coords[1] & x <= coords[2]) & (y >= coords[3] & y <= coords[4])
	
	return(g)
}


# summarise gaze by trial
gaze_summarise_by_trial <- function(
	sample = NULL,
	processed = NULL,
	google_email = NULL
){
	
	# if not provided, generate sample and processed gaze data
	if (is.null(processed)){
		if (is.null(sample)) {
			if (is.null(google_email)){
				google_email <- readline(prompt = "Google email: ")
				sample <- sample_get(google_email)
			} else {
				sample <- sample_get(google_email) %>%
					select(participant_id, id_db, location, age_group, lp, test_language, list, version)
			}
		}
		processed <- gaze_process(sample)
	}
	
	# summarise data
	gaze_summary_trial <- processed %>%
		group_by(participant, age_group, trial, lp, trial_type) %>%
		summarise(
			target = sum(target, na.rm = TRUE),
			distractor = sum(distractor, na.rm = TRUE),
			n = n(),
			.groups = "drop"
		) %>%
		rowwise() %>%
		mutate(
			p_target = prod(
				target,
				1/sum(target, distractor, na.rm = TRUE),
				na.rm = TRUE
			),
			p_distractor = prod(
				distractor,
				1/sum(target, distractor, na.rm = TRUE),
				na.rm = TRUE
			)
		) %>%
		ungroup()
	
	# return outcome
	return(gaze_summary_trial)
}

