library(readxl)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)

col_type_vct <- c("text", "text", "text",
				  "text", "text", "date",
				  "text", "text", "text",
				  "text", "text", "text",
				  "text", "text")

suppressWarnings({
	participants_oxf <- file.path("data-raw", 
								  "participants",
								  "Oxford_ESRC_participant.xlsx") |> 
		read_xlsx(.name_repair = make_clean_names, 
				  col_types = col_type_vct, progress = FALSE) |> 
		slice(-c(1, 45:50, 84:91, 149:155)) |> 
		rename(id = participant_id,
			   id_vocab = unique_id_for_cdi_matching,
			   age_group = test_cohort,
			   lp = group,
			   sex = gender,
			   date_test = dot,
			   comments = notes,
			   list = cp_ver) |> 
		mutate(id_session = id,
			   id = ifelse(!is.na(previous_id_if_applicable ),
						   previous_id_if_applicable,
						   id),
			   age_group = paste0(age_group, " months"),
			   sex = tolower(substr(sex, 1, 1)),
			   date_test = as.Date(date_test),
			   age = time_length(difftime(date_test, 
			   						   date_test - days(age_days)),
			   				  unit = "months"),
			   location = "Oxford",
			   doe_english = 1,
			   doe_catalan = 0,
			   doe_spanish = 0,
			   doe_others = 0,
			   test_language = "English",
			   version = "British",
			   lp = "Monolingual (English)") |>
		mutate(session = 1:n(), .by = id) |> 
		select(id, location, session, age_group, age, sex, lp, date_test, 
			   test_language, list, version, matches("doe"), 
			   id_vocab, id_session, comments) |> 
		arrange(id, session, age_group, date_test)
})

write_csv(participants_oxf,
		  file.path("data-raw", 
		  		  "participants",
		  		  "participants-oxford.csv"))
