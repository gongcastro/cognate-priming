# participants ----
get_participants <- function(){
	
	suppressMessages({
		# participants
		
		participants <- range_read(
			ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE",
			sheet = "barcelona", 
			na = ""
		) %>% # defined in arguments
			# NA -> FALSE
			mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
			mutate(
				age_group = factor(
					paste0(age_group, " months"),
					levels = paste0(c(21, 25, 30), " months")
				),
				date_test = as_date(date_test),
				lp = factor(lp, levels = c("Monolingual", "Bilingual")),
				test_language = factor(test_language, levels = c("Catalan", "Spanish")),
				list = as.integer(list)
			) %>% 
			# remove pilot participants
			filter(!pilot) %>% 
			# remove participants with no gaze data
			drop_na(
				participant,
				filename
			) %>% 
			select(
				participant,
				id_db,
				date_test, 
				lp, 
				doe_catalan,
				doe_spanish,
				doe_others,
				age_group,
				test_language,
				list, 
				version,
				filename
			)
	})
	
	write_csv_arrow(participants, "data/participants.csv")
	
	return(participants)
	
}
