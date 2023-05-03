# get participant information
get_participants <- function(){
	
	suppressMessages({
		# participants
		
		sheet_id <- "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE"
		participants <- range_read(ss = sheet_id, sheet = "barcelona", na = "") %>%
			mutate(
				across(where(is.logical), ~ifelse(is.na(.), FALSE, .)),
				age = time_length(interval(date_birth, date_test), unit = "months"),
				age_group = factor(paste0(age_group, " months"),
								   levels = paste0(c(21, 25, 30), " months")),
				date_test = as_date(date_test),
				lp = factor(lp, levels = c("Monolingual", "Bilingual")),
				test_language = factor(test_language, levels = c("Catalan", "Spanish")),
				list = as.integer(list)
			) %>% 
			filter(!pilot) %>% # remove pilot participants
			drop_na(id, filename) %>% # remove participants with no gaze data
			select(id = id_db, date_test, lp, doe_catalan, doe_spanish, doe_others,
				   age_group, age, test_language, list, version, filename)
	})
	
	return(participants)
	
}
