# utils vocabulary

import_vocabulary <- function(location = c("oxf", "bcn")){
	
	if ("bcn" %in% location){
		# vocabulary Barcelona
		ml_connect("gonzalo.garciadecastro@upf.edu")
		p <- ml_participants() 
		r <- ml_responses(p, update = TRUE)
		vocab_responses <- r %>% 
			mutate(
				understands = response > 1, # participant understands the item
				age_group = case_when(
					between(age, 20, 24) ~ 21,
					between(age, 24, 28) ~ 25,
					between(age, 28, 33) ~ 30
				)
			) %>% 
			filter(understands, study %in% "CognatePriming") %>% 
			select(id_exp, time, age_group, item, response) %>% 
			group_by(id_exp, time, age_group) %>% 
			summarise(understands = list(unique(item)), .groups = "drop")
		
		vocab_bcn <- ml_vocabulary(p, r, scale = "prop", by = c("id_exp")) %>% 
			right_join(select(p, id_db, time, id_exp)) %>% 
			filter(type=="understands") %>% 
			right_join(vocab_responses) %>% 
			select(
				id_db,
				vocab_size = vocab_prop_total,
				age_group
			) 
	}
	
	# vocabulary Oxford
	if("oxf" %in% location) {
		vocab_oxf <- read_xlsx(here("Data", "vocabulary_oxf.xlsx"), na = c("", "NA", "?")) %>% 
			mutate_at(vars(everything(), -item), as.integer) %>% 
			pivot_longer(-item, names_to = "id_db", values_to = "response") %>% 
			separate(item, c("category", "item"), sep = " - ") %>% 
			mutate(
				understands = response %in% c(1, 2),
				says = response %in% 2,
				age_group = case_when(
					substr(id_db, start = 1, stop = 2) %in% c(21) ~ 21,
					substr(id_db, start = 1, stop = 2) %in% c(24, 25) ~ 25,
					substr(id_db, start = 1, stop = 2) %in% c(27, 30) ~ 30
				)
			) %>% 
			group_by(id_db, age_group) %>% 
			summarise(
				vocab_size = mean(understands),
				.groups = "drop"
			)
	}
	
	# merge both
	x <- list(OXF = vocab_oxf, BCN = vocab_bcn) %>% 
		bind_rows(.id = "location")
	
	return(x)
}
