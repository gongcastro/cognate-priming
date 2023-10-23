# Process raw participant information

library(dplyr)
library(arrow)
# library(liubridate)

# set params -------------------------------------------------------------------

age_levels <- paste0(c(21, 25, 30), " months")

# Barcelona --------------------------------------------------------------------

files <- file.path("data-raw", "participants.csv")

participants_bcn <- read_csv_arrow(files,
								   na = c("NA", "")) |> 
	mutate(id = as.character(id_db),
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
		   across(c(list), as.integer)) |> 
	# remove pilot participants or participants with no gaze data
	filter(!pilot, !is.na(id), !is.na(filename)) |> 
	select(id, date_test, sex, lp, age_group, age,
		   doe_catalan, doe_spanish, doe_english, doe_others,
		   test_language, list, version, filename)

test_participants(participants_bcn)

# Oxford -----------------------------------------------------------------------

files <- file.path("data-raw", "participants-oxford.csv")

participants_oxf <- read_csv_arrow(files,
								   na = c("NA", "")) |> 
	rename(id_vocab = id_db) |> 
	mutate(across(where(is.logical), \(x) ifelse(is.na(x), FALSE, x)),
		   across(starts_with("date_"), dmy),
		   across(starts_with("doe_"), \(x) as.numeric(gsub("%", "", x)) * 0.01),
		   age = time_length(days(age_days), unit = "months"),
		   id_num = as.integer(substr(gsub("[A-z]", "", id), start = 1, stop = 2)),
		   age_group = factor(paste0(age_group, " months"), levels = age_levels),
		   across(c(starts_with("id")), as.character),
		   date_test = as_date(date_test),
		   test_language = factor(test_language, levels = c("English", "Spanish")),
		   list = as.integer(list),
		   doe_catalan = 0,
		   lp = factor(lp, levels = c("Monolingual", "Bilingual")),
		   version = "British",
		   sex = tolower(substring(gender, 1, 1)),
		   filename = paste0(id, ".csv")) |> 
	filter(lp=="Monolingual", !is.na(id), !is.na(list)) |> 
	select(id, date_test, sex, lp, 
		   age_group, age, 
		   doe_catalan, doe_spanish, doe_english, doe_others = doe_other,
		   test_language, list, version, filename) |> 
	arrange(desc(date_test))

test_participants(participants_oxf)


# joint dataset ----------------------------------------------------------------

participants <- list(Barcelona = participants_bcn,
					 Oxford = participants_oxf) |> 
	bind_rows(.id = "location") |> 
	relocate(location, .after = id)

arrow::write_csv_arrow(participants, file.path("data/participants.csv"))
