# participants

get_participants <- function(
	participants_bcn,
	participants_oxf
){
	
	suppressMessages({
		# participants
		participants_bcn <- participants_bcn %>%
			mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
			mutate(
				age_group = as.factor(paste0(age_group, " months")),
				date_test = as_date(date_test)
			) %>% 
			filter(!pilot) %>% 
			drop_na(filename) %>% 
			select(participant, id_db, sex, date_test, age_group, lp, test_language, list, version, filename)
		
		# Oxford data ----
		# import vocabulary data
		participants_oxf <- participants_oxf %>% 
			clean_names() %>% 
			rename(participant = id, lp = lang_group) %>% 
			mutate(id_db = participant, test_language = "English") %>% 
			relocate(id_db, .after = participant)
		
		# merge data ----
		participants <- list(Barcelona = participants_bcn, Oxford = participants_oxf) %>% 
			bind_rows(.id = "location") %>% 
			select(participant, id_db, date_test, location, lp, age_group, test_language, list, version, filename)
		
		
	})
	
	return(participants)
	
}