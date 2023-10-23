# get participant information
get_participants <- function(participants_file){
	
	age_levels <- paste0(c(21, 25, 30), " months")
	
	participants <- read_csv(participants_file,
							 na = c("NA", ""),
							 show_col_types = FALSE) |> 
		mutate(
			id = gsub("cognatepriming", "", id),
			across(starts_with("id"), as.character),
			across(where(is.logical), 
				   \(x) ifelse(is.na(x), FALSE, x)),
			across(starts_with("date_"), dmy),
			across(starts_with("doe_"),
				   \(x) as.numeric(gsub("%", "", x)) * 0.01),
			age = time_length(interval(date_birth, date_test), 
							  unit = "months"),
			age_group = factor(paste0(age_group, " months"),
							   levels = age_levels),
			date_test = as_date(date_test),
			lp = factor(lp, levels = c("Monolingual", "Bilingual")),
			test_language = factor(test_language, levels = c("Catalan", "Spanish")),
			across(c(id, list), as.integer)
		) |> 
		filter(!pilot) |> # remove pilot participants
		drop_na(id, filename)|> # remove participants with no gaze data
		select(id, id_db, date_test, sex, lp, 
			   doe_catalan, doe_spanish, doe_others,
			   age_group, age, test_language, 
			   list, version, filename)
	
	test_participants(participants)
	
	return(participants)
	
}

#' Get participant-level
get_participants <- function(participants_file_bcn,
							 participants_file_oxf){
	
	lp_levels <- c("Monolingual (English)", "Monolingual", "Bilingual")
	lang_levels <- c("Catalan", "English", "Spanish")
	sex_levels <- c("f", "m")
	
	participants_bcn <- read_csv(participants_file_bcn, 
								 na = c("NA", ""),
								 name_repair = janitor::make_clean_names,
								 show_col_types = FALSE) |> 
		mutate(across(matches("id"), as.character),
			   across(starts_with("date_"), dmy),
			   across(starts_with("doe_"), 
			   	   \(x) as.numeric(gsub("%", "", x)) * 0.01),
			   vocab_id_response = vocab_id,
			   valid_participant = as.logical(valid_participant)) |> 
		filter(!pilot, !is.na(child_id), !is.na(filename), valid_participant) |> 
		select(child_id, session, vocab_id, vocab_id_response,
			   date_test, date_birth, lp, age, sex, version,
			   doe_english, doe_spanish, doe_others,
			   test_language, list, filename) 
	
	
	participants_oxf <- participants_file_oxf |> 
		read_csv(name_repair = janitor::make_clean_names, 
				 show_col_types = FALSE,
				 progress = FALSE) |> 
		slice(-c(1, 45:50, 84:91, 149:155)) |> 
		rename(child_id = participant_id,
			   vocab_id = unique_id_for_cdi_matching,
			   age_group = test_cohort,
			   lp = group,
			   sex = gender,
			   date_test = dot,
			   comments = notes,
			   list = cp_ver) |> 
		mutate(across(starts_with("date_"), dmy),
			   across(matches("_id"), as.character),
			   id_session = child_id,
			   child_id = if_else(!is.na(previous_id_if_applicable),
			   				   previous_id_if_applicable,
			   				   child_id),
			   age_group = paste0(age_group, " months"),
			   sex = tolower(substr(sex, 1, 1)),
			   date_birth = date_test - days(age_days),
			   age = difftime(date_test, date_test - days(age_days)) |> 
			   	time_length(unit = "months"),
			   location = "Oxford",
			   list = as.integer(list),
			   doe_english = 1,
			   doe_catalan = 0,
			   doe_spanish = 0,
			   doe_others = 0,
			   test_language = "English",
			   version = "British",
			   lp = "Monolingual (English)") |>
		filter(!is.na(list), !is.na(age)) |> 
		mutate(session = 1:n(), .by = child_id) |> 
		select(child_id, session, vocab_id, date_test, lp, 
			   doe_english, doe_spanish, doe_others, version, date_birth,
			   test_language, list, sex) 
	
	participants <- list(Barcelona = participants_bcn,
						 Oxford = participants_oxf) |> 
		bind_rows(.id = "location") |> 
		mutate(across(where(is.logical), 
					  \(x) ifelse(is.na(x), FALSE, x)),
			   across(list, as.integer),
			   age = time_length(interval(date_birth, date_test), 
			   				  unit = "months"),
			   lp = factor(lp, levels = lp_levels),
			   sex = factor(sex, levels = sex_levels),
			   test_language = factor(test_language, levels = lang_levels)) |> 
		rowwise() |> 
		mutate(doe = list(across(matches("doe_")))) |> 
		ungroup() |> 
		select(child_id, location, date_test, age, sex, lp, doe,
			   test_language, list, version,
			   vocab_id, vocab_id_response, filename)
	
	test_participants(participants)
	
	return(participants)
	
}

#' Get participant information in Oxford
get_participants_oxf <- function(participants_file){
	
	
	
	
	return(participants)
	
}
