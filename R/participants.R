#' Get participant-level information
#'
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
			   age_group = paste0(age_group, " months"),
			   valid_participant = as.logical(valid_participant)) |> 
		filter(!pilot, !is.na(child_id), !is.na(filename), valid_participant) |> 
		select(child_id, session_id, session_n = session,
			   vocab_id, vocab_id_response,
			   date_test, date_birth, lp, age_group, age, sex, version,
			   matches("doe"), test_language, list, filename) 
	
	
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
		mutate(across(starts_with("date_"), 
					  \(x) suppressWarnings(dmy(x))),
			   across(matches("_id"), as.character),
			   age_group = paste0(age_group, " months"),
			   age_group = as.factor(age_group),
			   vocab_id_response = if_else(grepl("^R_", vocab_id),
			   							vocab_id, NA_character_),
			   session_id = child_id,
			   child_id = if_else(!is.na(previous_id_if_applicable),
			   				   previous_id_if_applicable,
			   				   child_id),
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
		mutate(session_n = 1:n(), .by = child_id) |> 
		select(child_id, session_id, session_n, vocab_id, vocab_id_response,
			   date_test, lp, age_group,
			   doe_english, doe_catalan, doe_spanish, doe_others, version, date_birth,
			   test_language, list, sex) 
	
	out <- list(Barcelona = participants_bcn,
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
		mutate(doe = list(unlist(across(matches("doe_"))))) |> 
		ungroup() |> 
		select(child_id, location, session_id, session_n, 
			   date_test, age_group, age, sex, lp, doe,
			   test_language, list, version,
			   vocab_id, vocab_id_response, filename)
	
	test_participants(out)
	
	return(out)
	
}

