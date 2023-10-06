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
	
	return(participants)
	
}
