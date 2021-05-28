# participants

# set up ----
library(tidyverse)
library(janitor)
library(multilex)
library(googlesheets4)
library(readxl)
library(multilex)
library(lubridate)
library(here)

# set params
ml_connect("gonzalo.garciadecastro@upf.edu") # log into MultiLex
cdi_replacements <- c("boots" = "boot", "boot(s)" = "boot", "fire engine" = "fireengine",
					  "teddy bear" = "teddy", "bunny / rabbit" = "bunny",
					  "lorry / truck" = "truck", "bicycle / bike" = "bike")

# Barcelona data ----
p <- ml_participants()
r <- ml_responses(p, update = FALSE)
vocab_raw_barcelona <- r %>% 
	mutate(
		understands = response > 1,
		age_group =  as.factor(
			case_when(
				between(age, 20, 24) ~ 21,
				between(age, 24, 28) ~ 25,
				between(age, 28, 33) ~ 30
			)),
		age_group = paste0(age_group, " months")
		) %>%
	filter(
		study=="CognatePriming",
		understands
	) %>%
	select("id_exp", "time", "age_group", "item", "response") %>%
	group_by(id_exp, time, age_group) %>%
	summarise(vocab_words = list(unique(item)), .groups = "drop")

vocabulary_barcelona <- ml_vocabulary(p, r, scale = "prop", by = c("id_exp")) %>%
	right_join(select(p, id_db, time, id_exp)) %>%
	filter(type=="understands") %>%
	right_join(vocab_raw_barcelona) %>%
	select(id_db, age_group, vocab_size = vocab_prop_total, vocab_words)

vocab_norms_barcelona <- r %>%
	filter(language==dominance, between(age, 19, 22)) %>% 
	mutate(understands = response > 1) %>% 
	group_by(item) %>% 
	summarise(prop = mean(understands, na.rm = TRUE), .groups = "drop") %>% 
	filter(prop > 0.5) %>% 
	pull(item)

# participants
participants_barcelona <- range_read(ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", sheet = "barcelona", na = "") %>%
	mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
	mutate(
		age_group = as.factor(paste0(age_group, " months")),
		   date_test = as_date(date_test)
		) %>% 
	filter(!pilot) %>% 
	left_join(vocabulary_barcelona) %>% 
	select(participant, date_test, age_group, lp, test_language, list, version, vocab_size, vocab_words, filename)

# Oxford data ----
# import vocabulary data
vocabulary_raw_oxford <- excel_sheets(here("Data", "Vocabulary", "vocabulary_oxford_Apr2021.xlsx")) %>%
	map(function(x){
		read_xlsx(
			here("Data", "Vocabulary", "vocabulary_oxford_Apr2021.xlsx"), 
			sheet = x, 
			na = c("", "NA", "?", "x")
		)
	}
	) %>%
	set_names(excel_sheets(here("Data", "Vocabulary", "vocabulary_oxford_Apr2021.xlsx"))) %>%
	bind_rows(.id = "version") %>%
	mutate_at(vars(-c(item, version)), as.integer) %>%
	pivot_longer(-c(item, version), names_to = "participant", values_to = "response") %>%
	#separate(item, c("category", "item"), sep = " - ", fill = "left") %>% 
	mutate(
		understands = response %in% c(1, 2),
		says = response %in% 2,
		# define age group
		age_group = case_when(
			substr(participant, start = 1, stop = 2) %in% c(21) ~ 21,
			substr(participant, start = 1, stop = 2) %in% c(24, 25) ~ 25,
			substr(participant, start = 1, stop = 2) %in% c(27, 30) ~ 30
		),
		age_group = as.factor(paste0(age_group, " months"))
	) %>%
	separate(item, c("category", "item"), sep = " - ", fill = "left") %>% 
	drop_na(response) %>%
	arrange(participant, version) %>% 
	select(-category) %>% 
	mutate(item = str_replace_all(item, cdi_replacements))

vocabulary_size_oxford <- vocabulary_raw_oxford %>%
	group_by(participant, age_group) %>%
	summarise(
		vocab_size = mean(understands), # proportion of understood words
		.groups = "drop"
	)

vocabulary_oxford <- vocabulary_raw_oxford %>%
	filter(understands) %>%
	group_by(participant, age_group) %>%
	summarise(
		vocab_words = list(unique(item)), # list of understood words
		.groups = "drop"
	) %>%
	right_join(vocabulary_size_oxford)

participants_oxford <- read_xlsx(here("Data", "Participants", "participant_oxford_Apr2021.xlsx")) %>% 
	clean_names() %>% 
	rename(participant = id, lp = lang_group) %>% 
	mutate(id_db = participant, test_language = "English") %>% 
	relocate(id_db, .after = participant) %>% 
	left_join(vocabulary_oxford)

# merge data ----
participants <- list(Barcelona = participants_barcelona, Oxford = participants_oxford) %>% 
	bind_rows(.id = "location")

# export data ----
saveRDS(vocab_norms_barcelona, here("Data", "Vocabulary", "vocabulary_norms.rds"))
saveRDS(participants, here("Data", "Participants", "participants.rds"))

