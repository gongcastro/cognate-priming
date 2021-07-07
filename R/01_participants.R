# participants

# set up ----
library(tidyverse)
library(janitor) # for cleaining column names
library(googlesheets4) # for participant data
library(readxl) # for importing Excel spreadsheets
library(lubridate) # for working with dates
library(here) # for reproducible file paths

# import data ----
# participants
participants_barcelona <- range_read(ss = "1JkhN4iBh3bi6PSReGGk9jSrVgDhZNOUmve6vNS2eEqE", sheet = "barcelona", na = "") %>%
	mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>%
	mutate(
		age_group = as.factor(paste0(age_group, " months")),
		date_test = as_date(date_test)
	) %>% 
	filter(!pilot) %>% 
	select(participant, id_db, sex, date_test, age_group, lp, test_language, list, version, filename)

# Oxford data ----
# import vocabulary data
participants_oxford <- read_xlsx(here("Data", "Participants", "participant_oxford_Apr2021.xlsx")) %>% 
	clean_names() %>% 
	rename(participant = id, lp = lang_group) %>% 
	mutate(id_db = participant, test_language = "English") %>% 
	relocate(id_db, .after = participant)

# merge data ----
participants <- list(Barcelona = participants_barcelona, Oxford = participants_oxford) %>% 
	bind_rows(.id = "location") %>% 
	select(participant, id_db, date_test, location, lp, age_group, test_language, list, version, filename)

# export data ----
saveRDS(participants, here("Data", "Participants", "participants.rds"))

