# get participant information
get_participants <- function(participants_bcn_file,
							 participants_oxf_file){
	
	# set factor levels
	age_levels <- paste0(c(21, 25, 30), " months")
	lp_levels <- c("Monolingual", "Bilingual", "Monolingual (English)")
	lang_levels<- c("Catalan", "Spanish", "English")
	location_levels <- c("Barcelona", "Oxford")
	
	participants_bcn <- read_csv(participants_bcn_file,
								 na = c("NA", ""),
								 show_col_types = FALSE) |> 
		mutate(id = id_vocab,
			   id_vocab = id_vocab_response,
			   across(starts_with("id"), as.character),
			   across(where(is.logical), 
			   	   \(x) ifelse(is.na(x), FALSE, x)),
			   across(starts_with("date_"), dmy),
			   across(starts_with("doe_"),
			   	   \(x) as.numeric(gsub("%", "", x)) * 0.01),
			   age = time_length(interval(date_birth, date_test), 
			   				  unit = "months"),
			   age_group = paste0(age_group, " months"),
			   date_test = as_date(date_test),
			   id_session = paste0(id, "_", session),
			   across(c(id, list), as.integer)) |> 
		filter(!pilot) |> # remove pilot participants
		drop_na(id, filename)|> # remove participants with no gaze data
		select(id, location, session, age_group, age, sex,
			   lp, date_test, test_language, list, version, 
			   doe_english, doe_catalan, doe_spanish, doe_others,
			   id_vocab = id_vocab_response, id_session,
			   comments, filename)
	
	participants_oxf <- read_csv(participants_oxf_file,
								 na = c("NA", ""),
								 show_col_types = FALSE) |> 
		mutate(across(starts_with("id"), as.character),
			   across(where(is.logical), 
			   	   \(x) ifelse(is.na(x), FALSE, x)),
			   across(starts_with("doe_"),
			   	   \(x) as.numeric(gsub("%", "", x)) * 0.01),
			   date_test = as_date(date_test),
			   across(c(id, list), as.integer)) |> 
		drop_na(list, date_test, id)|> # remove participants with no gaze data
		select(id, location, session, age_group, age, sex,
			   lp, date_test, test_language, list, version, 
			   doe_english, doe_catalan, doe_spanish, doe_others,
			   id_vocab, id_session,
			   comments)
	
	participants <- bind_rows(participants_bcn,
							  participants_oxf) |> 
		mutate(id = as.character(id),
			   location = factor(location, levels = location_levels),
			   age_group = factor(age_group, levels = age_levels),
			   lp = factor(lp, levels = lp_levels),
			   test_language = factor(test_language, levels = lang_levels))
	
	test_participants(participants)
	
	return(participants)
	
}


