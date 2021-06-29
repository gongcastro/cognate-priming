# stimuli

# set up ----

# load packages
library(tidyverse)
library(readxl)
library(multilex)
library(janitor)
library(here)

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
	rename(word = spelling, frequency = log_freq_zipf) %>% 
	select(word, frequency)

# Catalan SUBTLEX: https://psico.fcep.urv.cat/projectes/gip/papers/SUBTLEX-CAT.xlsx
subtlex_cat <- read_xlsx(here("Data", "Stimuli", "SUBTLEX-CAT.xlsx")) %>% 
	clean_names() %>% 
	rename(word = words, frequency = zipf) %>% 
	select(word, frequency)

# Spanish SUBTLEX: http://crr.ugent.be/papers/SUBTLEX-ESP.zip
subtlex_spa <- read_xlsx(here("Data", "Stimuli", "SUBTLEX-ESP.xlsx"), .name_repair = "minimal") %>% 
	clean_names() %>% 
	select(-starts_with("x"))
subtlex_spa <- tibble(
	word = unlist(subtlex_spa[, c(1, 5, 9)]),
	freq_count = unlist(subtlex_spa[, c(2, 6, 10)]),
	freq_per_million = unlist(subtlex_spa[, c(3, 7, 11)]),
	log_freq = unlist(subtlex_spa[, c(4, 8, 12)])
) %>% 
	mutate(frequency = log10(freq_per_million)+3) %>% 
	select(word, frequency)

# merge
subtlex <- bind_rows(
	list(Spanish = subtlex_spa, Catalan = subtlex_cat, English = subtlex_eng),
	.id = "test_language"
) %>% 
	distinct(test_language, word, .keep_all = TRUE)

# familiarity norms ----
p <- ml_participants()
r <- ml_responses(p, update = FALSE)
words <- unique(c(trials[trials$location=="Barcelona",]$prime_cdi, trials[trials$location=="Barcelona",]$target_cdi))
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
	rename(frequency_prime = frequency) %>% 
	left_join(subtlex, by = c("target" = "word", "test_language")) %>% 
	rename(frequency_target = frequency) %>% 
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

