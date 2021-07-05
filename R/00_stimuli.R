# stimuli

# set up ----

# load packages
library(tidyverse)
library(readxl) # for importing Excel spreadsheets
library(multilex) # for extracting familiarity norms
library(janitor) # for cleaning column names
library(childesr) # for extracting frequencies from CHILDES
library(here) # for reproducible file paths

# set params
ml_connect("gonzalo.garciadecastro@upf.edu")

# import data ----
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx"))

# semantic category ----
pool <- select(multilex::pool, item, language, category) %>%
	rename(test_language = language)

# animacy ----
animacy <- read.csv(here("Data", "Stimuli", "animacy.csv")) %>% 
	as_tibble() %>% 
	mutate(is_animate = as.logical(is_animate))

# lexical frequency norms ----
# UK SUBTLEX: http://crr.ugent.be/papers/SUBTLEX-UK.xlsx
subtlex_eng <- read_xlsx(here("Data", "Stimuli", "SUBTLEX-UK.xlsx")) %>% 
	clean_names() %>% 
	rename(word = spelling, frequency_subtlex = log_freq_zipf) %>% 
	select(word, frequency_subtlex)

# Catalan SUBTLEX: https://psico.fcep.urv.cat/projectes/gip/papers/SUBTLEX-CAT.xlsx
subtlex_cat <- read_xlsx(here("Data", "Stimuli", "SUBTLEX-CAT.xlsx")) %>% 
	clean_names() %>% 
	rename(word = words, frequency_subtlex = zipf) %>% 
	select(word, frequency_subtlex)

# Spanish SUBTLEX: http://crr.ugent.be/papers/SUBTLEX-ESP.zip
subtlex_spa <- read_xlsx(here("Data", "Stimuli", "SUBTLEX-ESP.xlsx"), .name_repair = "minimal") %>% 
	clean_names() %>% 
	select(-starts_with("x"))
subtlex_spa <- tibble(
	word = unlist(subtlex_spa[, c(1, 5, 9)]),
	freq_count_subtlex = unlist(subtlex_spa[, c(2, 6, 10)]),
	freq_per_million_subtlex = unlist(subtlex_spa[, c(3, 7, 11)]),
	log_freq_subtlex = unlist(subtlex_spa[, c(4, 8, 12)])
) %>% 
	mutate(frequency_subtlex = log10(freq_per_million_subtlex)+3) %>% 
	select(word, frequency_subtlex)

# merge
subtlex <- bind_rows(
	list(Spanish = subtlex_spa, Catalan = subtlex_cat, English = subtlex_eng),
	.id = "test_language"
) %>% 
	distinct(test_language, word, .keep_all = TRUE)


# lexical frequency norms (CHILDES) ----
childes_n <- get_speaker_statistics(collection = unique(childes_tokens$collection_name)) %>% 
	filter(str_detect(language, "spa|cat|eng")) %>%
	group_by(language) %>% 
	summarise(num_tokens = sum(num_tokens), .groups = "drop") %>% 
	mutate(language = str_split(language, " ")) %>% 
	summarise(
		num_tokens_eng = sum(num_tokens["eng" %in% language]),
		num_tokens_spa = sum(num_tokens["spa" %in% language]),
		num_tokens_cat = sum(num_tokens["cat" %in% language])
	) %>% 
	unlist()

childes <- get_tokens(
	role = "target_child",
	token =  distinct(trials, prime, target) %>% unlist() %>% unique()
) %>% 
	mutate(gloss = str_to_lower(gloss)) %>% 
	filter(str_detect(language, "spa|cat|eng")) %>%
	count(gloss, language) %>% 
	mutate(language = str_split(language, " ")) %>% 
	group_by(gloss) %>% 
	summarise(
		english = sum(n["eng" %in% language]),
		spanish = sum(n["spa" %in% language]),
		catalan = sum(n["cat" %in% language])
	) %>% 
	pivot_longer(-gloss, names_to = "language", values_to = "freq_counts") %>% 
	filter(freq_counts>0) %>% 
	mutate(
		language = str_to_sentence(language),
		freq_per_million = case_when(
			language=="English" ~ freq_counts/childes_n["num_tokens_eng"]*1e6,
			language=="Catalan" ~ freq_counts/childes_n["num_tokens_cat"]*1e6,
			language=="Spanish" ~ freq_counts/childes_n["num_tokens_spa"]*1e6
		),
		frequency_childes = log10(freq_per_million)+3
	) %>% 
	rename(word = gloss, test_language = language) %>% 
	select(word, test_language, frequency_childes)


# familiarity norms ----
p <- ml_participants()
r <- ml_responses(p, update = TRUE)
words <- unique(c(
	trials[trials$location=="Barcelona",]$prime_cdi,
	trials[trials$location=="Barcelona",]$target_cdi
))
words <- sort(words[!grepl("eng_", words)])
familiarity <- ml_norms(
	p, r, 
	norms_type = "understands",
	norms_item = words,
	norms_age = c(19, 23)
) %>% 
	filter(item_dominance=="L1") %>% 
	group_by(item) %>% 
	summarise(
		yes = sum(yes, na.rm = TRUE),
		n = sum(n, na.rm = TRUE),
		.groups = "drop"
	) %>% 
	# adjusted proportion (Gelman, Hill & Vehtari, 2020)
	mutate(
		familiarity = (yes+2)/(n+4),
		familiarity_se = sqrt(familiarity*(1-familiarity)/(n+4)),
		familiarity_ci_lower = familiarity + qnorm((1-0.95)/2)*familiarity_se,
		familiarity_ci_upper = familiarity + qnorm(1-(1-0.95)/2)*familiarity_se,
		familiarity_ci_lower = ifelse(familiarity_ci_lower<0, 0, familiarity_ci_lower), # truncate at 0
		familiarity_ci_upper = ifelse(familiarity_ci_upper>1, 1, familiarity_ci_upper) # truncate at 0
	) %>% 
	select(item, starts_with("familiarity"))

# merge data ----
stimuli <- trials %>% 
	left_join(pool, by = c("target_cdi" = "item", "test_language")) %>% 
	rename(category_prime = category) %>% 
	left_join(pool, by = c("prime_cdi" = "item", "test_language")) %>% 
	rename(category_target = category) %>% 
	left_join(subtlex, by = c("prime" = "word", "test_language")) %>% 
	rename(frequency_prime_subtlex = frequency_subtlex) %>% 
	left_join(subtlex, by = c("target" = "word", "test_language")) %>% 
	rename(frequency_target_subtlex = frequency_subtlex) %>% 
	left_join(childes, by = c("prime" = "word", "test_language")) %>% 
	rename(frequency_prime_childes = frequency_childes) %>% 
	left_join(childes, by = c("target" = "word", "test_language")) %>% 
	rename(frequency_target_childes = frequency_childes) %>% 
	left_join(animacy, by = c("prime" = "object", "test_language")) %>% 
	rename(is_animate_prime = is_animate) %>% 
	left_join(animacy, by = c("target" = "object", "test_language")) %>% 
	rename(is_animate_target = is_animate) %>% 
	left_join(familiarity, c("prime_cdi" = "item")) %>% 
	rename_at(vars(starts_with("familiarity")), 
			  function(x) paste0(x, "_prime")) %>% 
	left_join(familiarity, c("target_cdi" = "item")) %>% 
	rename_at(vars(starts_with("familiarity") & !ends_with("_prime")), 
			  function(x) paste0(x, "_target")) %>% 
	rename_all(function(x) str_replace(x, "prime_target", "prime"))

# export data ----
saveRDS(stimuli, here("Data", "Stimuli", "stimuli.rds"))

