# vocabulary size

# set up ----

# load packages
library(tidyverse)
library(mice) # for multiple imputation
library(multilex) # for extracting Barcelona vocabulary data
library(readxl) # for importing Excel spradsheets
library(here) # for reproducible file paths

# set params
ml_connect("gonzalo.garciadecastro@upf.edu") # log into MultiLex
cdi_replacements <- c("boots" = "boot", "boot(s)" = "boot", "fire engine" = "fireengine",
					  "teddy bear" = "teddy", "bunny / rabbit" = "bunny",
					  "lorry / truck" = "truck", "bicycle / bike" = "bike")

participants <- readRDS(here("Data", "Participants", "participants.rds"))

# Barcelona vocabulary ----
p <- ml_participants()
r <- ml_responses(p, update = FALSE)
l <- ml_logs(p, r) %>% 
	rename(participant = id_exp) %>% 
	mutate(
		age_group =  as.factor(
			case_when(
				between(age, 19, 24) ~ 21,
				between(age, 24, 28) ~ 25,
				between(age, 28, 34) ~ 30
			)),
		age_group = paste0(age_group, " months")
	) %>% 
	filter(age_group != "NA months") %>% 
	select(participant, time, age_group, lp)
v <- ml_vocabulary(p, r, scale = "prop", by = "id_exp") %>% 
	filter(type=="understands")

# norms (by item)
vocab_items_barcelona <- r %>% 
	rename(participant = id_exp) %>% 
	mutate(
		understands = response > 1,
		age_group =  as.factor(
			case_when(
				between(age, 19, 24) ~ 21,
				between(age, 24, 28) ~ 25,
				between(age, 28, 34) ~ 30
			)),
		age_group = paste0(age_group, " months")
	) %>%
	filter(study=="CognatePriming", age_group != "NA months", understands) %>%
	select(participant, time, age_group, item, response) %>%
	group_by(participant, age_group) %>%
	summarise(vocab_words = list(unique(item)), .groups = "drop")

# norms (by participant)
vocab_size_barcelona <- v %>%
	rename(participant = id_exp) %>% 
	right_join(select(l, participant, time)) %>% 
	mutate(
		age_group =  as.factor(
			case_when(
				between(age, 19, 24) ~ 21,
				between(age, 24, 28) ~ 25,
				between(age, 28, 34) ~ 30
			)),
		age_group = paste0(age_group, " months")
	) %>%
	filter(age_group != "NA months") %>% 
	select(
		participant, age_group,
		vocab_size_total = vocab_prop_total,
		vocab_size_l1 = vocab_prop_dominance_l1,
		vocab_size_conceptual = vocab_prop_conceptual
	)

# impute vocab size
vocab_size_imputed_barcelona <- full_join(
	select(participants, participant, age_group, lp),
	vocab_size_barcelona
) %>% 
	mutate(is_imputed = is.na(vocab_size_l1) | is.na(vocab_size_total) | is.na(vocab_size_conceptual)) %>% 
	mice(m = 5, method = "pmm", seed = 888) %>% 
	complete() %>% 
	as_tibble()

# item norms (by item in whole database)
vocab_norms_item_barcelona <- r %>%
	rename(participant = id_exp) %>% 
	left_join(l) %>% 
	filter(language==dominance) %>% 
	mutate(
		understands = response > 1,
		age_group =  as.factor(
			case_when(
				between(age, 19, 24) ~ 21,
				between(age, 24, 28) ~ 25,
				between(age, 28, 34) ~ 30
			)),
		age_group = paste0(age_group, " months")
	) %>%
	filter(age_group != "NA months") %>% 
	group_by(item, age_group, lp) %>% 
	summarise(
		prop = mean(understands, na.rm = TRUE),
		n = n(),
		.groups = "drop") %>% 
	filter(prop > 0.5) %>% 
	group_by(age_group, lp) %>%
	summarise(vocab_words = list(unique(item)), .groups = "drop")


vocab_barcelona <- participants %>% 
	filter(location=="Barcelona") %>% 
	select(participant, age_group) %>% 
	left_join(vocab_size_imputed_barcelona) %>% 
	left_join(vocab_items_barcelona) %>% 
	mutate(is_imputed_vocab_words = map_lgl(vocab_words, is.null)) %>% 
	group_split(is_imputed_vocab_words) 
vocab_barcelona[[2]] <- vocab_barcelona[[2]] %>% 
	select(-vocab_words) %>% 
	left_join(vocab_norms_item_barcelona)
vocab_barcelona <- bind_rows(vocab_barcelona) %>% 
	select(-c(lp, is_imputed_vocab_words))
	
# Oxford vocabulary ----
vocab_raw_oxford <- excel_sheets(here("Data", "Vocabulary", "vocabulary_oxford_Apr2021.xlsx")) %>%
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

vocab_size_oxford <- vocab_raw_oxford %>%
	group_by(participant, age_group) %>%
	summarise(
		vocab_size_total = mean(understands, na.rm = TRUE), # proportion of understood words
		n = n(),
		.groups = "drop"
	) %>% 
	mutate(
		vocab_size_total = ifelse(n < 100, NA, vocab_size_total), # remove incomplete vocabulary responses
		vocab_size_l1 = vocab_size_total,
		vocab_size_conceptual = vocab_size_total
	)

vocab_oxford <- vocab_raw_oxford %>%
	filter(understands) %>%
	group_by(participant, age_group) %>%
	summarise(
		vocab_words = list(unique(item)), # list of understood words
		.groups = "drop"
	) %>%
	right_join(vocab_size_oxford) %>% 
	mutate(is_imputed = FALSE) %>% 
	select(-n)

# merge datasets ----
vocab <- bind_rows(vocab_barcelona, vocab_oxford)
saveRDS(vocab, here("Data", "Vocabulary", "vocabulary.rds"))
