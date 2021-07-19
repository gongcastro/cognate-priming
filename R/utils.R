# utils

# custom ggplot theme
theme_custom <- function(){
	theme(
		panel.background = element_rect(fill = "transparent"),
		panel.grid = element_blank(),
		#panel.grid = element_line(colour = "grey", linetype = "dotted"),
		panel.border = element_rect(fill = "transparent", colour = "black"),
		text = element_text(colour = "black", size = 15),
		axis.text = element_text(colour = "black")
	)
}

# get multilex data ----
get_credentials <- function(
	google_email = "gonzalo.garciadecastro@upf.edu",
	formr_email = "gonzalo.garciadecastro@upf.edu"
){
	ml_connect(
		google_email,
		formr_email,
		formr_password = key_get("formr", "gonzalo.garciadecastro@upf.edu")
	)
}

get_multilex <- function(
	update = FALSE,
	type = "understands"
){
	get_credentials()
	
	p <- ml_participants()
	r <- ml_responses(p, update = update)
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
		filter(type==type)
	
	m <- list(
		participants = p, responses = r, logs = l,
		vocabulary = v, pool = multilex::pool
	)
	
	return(m)
}

# adjusted proportion, SE, and CI ----
# from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)
prop_adj_se <- function(y, n) {
	prop <- prop_adj(y, n)
	sqrt(prop*(1-prop)/(n+4))
}
prop_adj_ci <- function(y, n, conf = 0.95) {
	prop <- (y+2)/(n+4)
	se <- sqrt(prop*(1-prop)/(n+4))
	ci <-  prop + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
	ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
	ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
	return(ci)
}

# transform logit scale to probability
logit_to_prob <- function(x) exp(x) / (1 + exp(x))

# get familiarity ----
get_familiarity <- function(
	tokens, # words to retrieve familiarity for
	type = "understands", # understands or produces
	update = TRUE, # should vocabulary data be updated
	conf = 0.95, # confidence level of the estimates
	oxford_data, # Oxford familiarity data
	multilex_data
){
	
	familiarity_bcn <- ml_norms(
		multilex_data$participants, multilex_data$responses, 
		norms_type = type,
		norms_item = tokens, 
		norms_age = c(17, 19)
	) %>% 
		filter(item_dominance=="L1") %>% 
		group_by(item) %>% 
		summarise(
			yes = sum(yes, na.rm = TRUE),
			n = sum(n, na.rm = TRUE),
			.groups = "drop"
		) 
	
	familiarity_oxf <- oxford_data %>%
		distinct(item, .keep_all = TRUE) %>% 
		rename(
			familiarity = `18ocdi_comp`,
			n = norms_n
		) %>% 
		mutate(
			item = paste0("eng_", item),
			yes = n*familiarity
		) %>% 
		select(item, yes, n)
	
	familiarity <- bind_rows(familiarity_bcn, familiarity_oxf) %>% 
		mutate(
			familiarity = prop_adj(yes, n),
			familiarity_se = prop_adj_se(yes, n)
		) %>% 
		select(item, starts_with("familiarity")) %>% 
		rename(word = item) %>% 
		mutate(
			test_language = case_when(
				grepl("eng_", word) ~ "English",
				grepl("cat_", word) ~ "Catalan",
				grepl("spa_", word) ~ "Spanish"
			)) %>% 
		relocate(test_language, .before = word)
	
	return(familiarity)
}


# get SUBTLEX lexical frequencies ----
get_frequency_subtlex <- function(
	token,
	languages = c("spa", "cat", "eng")
){
	suppressMessages({
		
		subtlex_eng <- NULL
		subtlex_cat <- NULL
		subtlex_esp <- NULL
		
		if ("eng" %in% languages){
			subtlex_eng <- fread(
				"http://crr.ugent.be/papers/SUBTLEX-UK.txt",
				verbose = FALSE, showProgress = FALSE
			) %>%
				as_tibble() %>% 
				clean_names() %>% 
				rename(word = spelling, frequency = log_freq_zipf) %>% 
				select(word, frequency)
		}
		
		if ("spa" %in% languages){
			tf <- tempfile(fileext = "zip")
			GET("http://crr.ugent.be/papers/SUBTLEX-ESP.zip", write_disk(tf))
			subtlex_spa <- read_excel(unzip(tf)) %>% 
				clean_names() %>%
				select(starts_with("word_"), starts_with("log_")) %>% 
				rename_at(vars(starts_with("log")), ~gsub("log_freq", "frequency", .)) %>% 
				drop_na() 
			colnames(subtlex_spa) <- paste(rep(c("word", "frequency"), each = 3), 1:3, sep = "_")
			subtlex_spa <- pivot_longer(
				subtlex_spa,
				everything(),
				names_to = c(".value", "set"),
				names_pattern =  "(.)_(.)"
			) %>% 
				select(d, y)
			colnames(subtlex_spa) <- c("word", "frequency")
		}
		
		if("cat" %in% languages){
			tf <- tempfile(fileext = "xlsx")
			GET("https://psico.fcep.urv.cat/projectes/gip/papers/SUBTLEX-CAT.xlsx", write_disk(tf))
			subtlex_cat <- read_excel(tf) %>% 
				clean_names() %>%
				rename(word = words, frequency = zipf) %>% 
				select(word, frequency)
		}
		
		frequency <- list(English = subtlex_eng, Spanish = subtlex_spa, Catalan = subtlex_cat) %>% 
			bind_rows(.id = "test_language") %>% 
			select(word, test_language, frequency) %>% 
			filter(word %in% token)
	})
	return(frequency)
}

# get frequencies from CHILDES ----
# WARNING: especial characters are not being handled very well
get_frequency_childes <- function(
	token, # word(s) form to look up, e.g. c("table", "mesa")
	languages = c("eng", "cat", "spa"), # languages in which to look up the word form 
	... # other arguments (see ?childesr::get_speaker_statistics)
){
	
	suppressMessages({
		library(dplyr)
		library(stringr)
		library(tidyr)
		library(childesr)
		
		# get total number of tokens in each language
		total_counts <- get_speaker_statistics(...) %>%
			filter(str_detect(language, paste(languages, collapse = "|"))) %>%
			group_by(language) %>%
			summarise(num_tokens = sum(num_tokens), .groups = "drop") %>%
			mutate(language = str_split(language, " ")) %>%
			unnest(cols = language) %>%
			group_by(language) %>%
			summarise(n = sum(num_tokens, na.rm = TRUE), .groups = "drop")
		
		# absolute frequency (raw counts)
		freq_counts <- get_tokens(role = "target_child", token =  token, language = languages) %>%
			mutate(gloss = str_to_lower(gloss)) %>%
			filter(str_detect(language, paste(languages, collapse = "|"))) %>%
			count(gloss, language) %>%
			mutate(language = str_split(language, " ")) %>%
			unnest(language) %>%
			group_by(language, gloss) %>%
			summarise(freq_counts = sum(n), .groups = "drop") %>%
			filter(freq_counts>0)
		
		# relative frequency (counts per million)
		frequency <- freq_counts %>%
			left_join(total_counts, by = "language") %>%
			mutate(
				freq_per_million = freq_counts/n*1e6,
				freq_zipf = log10(freq_per_million)+3,
				language = str_replace_all(
					language, c("eng" = "English", "spa" = "Spanish", "cat" = "Catalan")
				)
			) %>%
			rename(word = gloss, test_language = language, frequency = freq_zipf) %>%
			select(word, test_language, frequency)
		
	})
	return(frequency)
}


# preprocess gaze data
get_gaze_raw <- function(
	file_paths
){
	relevant.variables <- c(
		"participant", "trial_num", "trial", "phase", "time",
		"l_x", "l_y", "l_v", "r_x", "r_y", "r_v"
	)
	colname.changes <- c(
		"system_time_stamp" = "time", "l_1" = "l_x", "l_2" = "l_y",
		"process" = "phase", "r_1" = "r_x", "r_2" = "r_y",
		"l_user_coord_3" = "l_user_coord_z",
		"r_user_coord_3" = "r_user_coord_z","suje_num" = "participant"
	)
	
	suppressMessages({
		raw <- file_paths %>% 
			fread(sep = ",", na.strings = c("", "NaN", "NA", "<NA>")) %>%
			# rename all variables to snake case
			clean_names() %>% 
			# fix some values from outdates files
			rename_all(str_replace_all, colname.changes) %>% 
			mutate(phase = str_replace_all(
				phase,
				c("GETTER" = "Getter",
				  "PRIMEIMAGE" = "Prime",
				  "TARGET_DISTRACTOR" = "Target-Distractor",
				  "prime" = "Prime",
				  "primeimage" = "Prime",
				  "target_distractor" = "Target-Distractor"
				))) %>%
			# get gaze in prime and target-distractor phases
			filter(phase %in% c("Target-Distractor", "Prime")) %>%
			mutate(
				participant = paste0("cognatepriming", participant),
				# fix the timestamp in some files
				time = row_number()*(1000/120)
			) %>%
			# change variables to the right class
			mutate_all(function(x) ifelse(is.nan(x), NA_real_, x)) %>% # NaNs to NAs
			mutate_at(vars(starts_with("l_"), starts_with("r_")), as.numeric) %>% # gaze coords to numeric
			mutate_at(vars(contains("_v")), as.logical) %>% # validity columns to logicals
			# get only variables of interest
			select(any_of(relevant.variables))
	})
}