#' Get vocabulary data
get_vocabulary <- function(participants, 
						   bvq_data, 
						   vocabulary_supp_bcn_file,
						   vocabulary_file_oxf,
						   cdi_file_oxf) {
	
	vocab_bcn <- get_vocabulary_bcn(participants, bvq_data, vocabulary_supp_bcn_file)
	vocab_oxf <- get_vocabulary_oxf(vocabulary_file_oxf,
									participants,
									cdi_file_oxf)
	
	vocabulary <- bind_rows(vocab_bcn, vocab_oxf)
	
	return(vocabulary)
}

# vocabulary size
get_vocabulary_bcn <- function(participants, # participants dataset (get_participants output)
							   bvq_data,
							   vocabulary_supp_bcn_file
){
	
	# vocabulary sizes BVQ database and Cognate Priming
	participants_tmp <- participants |> 
		filter(location=="Barcelona") |> 
		select(child_id, session_id, vocab_id, age, lp)
	
	vocab_supp <- get_vocabulary_supp_bcn(vocabulary_supp_bcn_file) 
	
	vocabulary <- bvq_data$vocabulary |>
		mutate(vocab_id = response_id) |> 
		right_join(select(participants_tmp, session_id, child_id, vocab_id, age, lp),
				   by = join_by(vocab_id)) |> 
		rowwise() |> 
		mutate(is_imputed = any(is.na(c_across(total_prop:te_prop)))) |> 
		ungroup() |> 
		relocate(child_id, vocab_id, age, is_imputed) |> 
		left_join(select(participants_tmp, child_id, session_id, vocab_id, age, lp),
				  by = join_by(child_id, session_id, vocab_id, age, lp)) |> 
		# multiple imputation
		impute_vocabulary(cols_impute = c("total_prop", "l1_prop",
										  "l2_prop", "concept_prop",
										  "te_prop"), 
						  cols_predictor = c("age", "lp"), 
						  bvq_data = bvq_data) |>
		# merge both datasets
		right_join(participants_tmp, imputed,
				   by = join_by(child_id, vocab_id, session_id, lp, age)) |> 
		mutate(lp = factor(lp, levels = c("Monolingual", "Bilingual"))) |> 
		select(child_id, session_id, vocab_id, is_imputed, matches("prop|count|contents")) |> 
		distinct(child_id, session_id, vocab_id, .keep_all = TRUE) |> 
		mutate(add_vocab_supp = map_int(contents, length) < 1) |> 
		left_join(vocab_supp, by = join_by(session_id)) |> 
		mutate(contents = if_else(add_vocab_supp & !is.na(contents_supp),
								  contents_supp, contents)) |> 
		select(-matches("supp"))
	
	
	# test_vocabulary(vocabulary)
	
	return(vocabulary)
}

#' Get vocabulary supplement
#' 
get_vocabulary_supp_bcn <- function(vocabulary_supp_bcn_file) {
	
	x <- vocabulary_supp_bcn_file |> 
		set_names(c("Catalan", "Spanish")) |> 
		map_dfr(function(x) {
			read_csv(file = x,
					 show_col_types = FALSE, 
					 name_repair = janitor::make_clean_names) |> 
				mutate(across(2:last_col(), as.character)) |> 
				pivot_longer(-x,
							 names_to = "session_id",
							 values_to = "response") |>
				mutate(response = !is.na(response),
					   session_id = gsub("x", "", session_id)) |> 
				rename(item = x)
		}, .id = "language") |> 
		summarise(contents_supp = list(item[response]),
				  .by = session_id)
	
	return(x)
}

#' Impute vocabulary size based on the whole BVQ database
impute_vocabulary <- function(x,
							  cols_impute,
							  cols_predictor, 
							  bvq_data,
							  ...) {
	
	# check args
	key.cols <- c(cols_predictor, cols_impute)
	is.col <- key.cols %in% names(x)
	if (!all(is.col)) {
		which.missing <- key.cols[!is.col]
		cli_abort("variable{?s} {which.missing} not found")
	}
	
	# prepare dataset for imputation
	x_tmp <- x[, c(key.cols)]
	db <- inner_join(bvq_data$vocabulary, 
					 bvq_data$logs,
					 by = join_by(response_id))
	db <- db[, c(key.cols)]
	to.impute <- rbind(x_tmp, db)
	
	# indicate where to impute data
	imp.index <- to.impute
	imp.index[,] <- FALSE
	imp.index[, cols_impute] <- is.na(to.impute[, cols_impute])
	
	# initiate MICE
	suppressWarnings({
		init <- mice::mice(to.impute, max = 0, print = FALSE)
	})
	
	# select predictors
	preds <- init$predictorMatrix
	preds[,] <- 0
	preds[, cols_predictor] <- 1
	
	# constrain proportions between 0 and 1
	post <- init$post
	post[cols_impute] <- "imp[[j]][,i] <- squeeze(imp[[j]][, i], c(0, 1))"
	
	# impute missing data
	suppressWarnings({
		imp <- mice::mice(to.impute, 
						  printFlag = FALSE,
						  where = imp.index,
						  predictorMatrix = preds,
						  post = post,
						  m = 5,
						  maxit = 20,
						  seed = 1234)
	})
	
	# get completed dataset
	imp.complete <- mice::complete(imp)
	imp.complete <- imp.complete[1:nrow(x_tmp), cols_impute]
	x_out <- x
	x_out[, cols_impute] <- imp.complete
	
	return(x_out)
	
}



# Oxford functions -------------------------------------------------------------

#' Import and process vocabulary data
get_vocabulary_oxf <- function(vocabulary_file,	participants,cdi_file_oxf) {
	
	participants_tmp <- participants |> 
		filter(location=="Oxford") |> 
		select(matches("_id"))
	
	stimuli_cdi_oxf <- read_csv(cdi_file_oxf,
								show_col_types = FALSE,
								na = c("", NA)) |> 
		pivot_longer(matches("item"),
					 names_to = "version",
					 values_to = "item_cdi") |> 
		group_split(version) |> 
		set_names(c("full", "supp"))
	
	cdi_full <- get_cdi_full_oxf(vocabulary_file) |> 
		left_join(stimuli_cdi_oxf$full, by = c("item" = "item_cdi")) |> 
		mutate(item = if_else(!is.na(stimulus), stimulus, item)) |> 
		rename(vocab_id = unique_id,
			   vocab_id_response = response_id) |> 
		summarise(total_prop = mean(response),
				  contents = list(item[response]),
				  .by = c(vocab_id, vocab_id_response)) |> 
		left_join(select(participants_tmp, session_id, vocab_id),
				  by = join_by(vocab_id)) |> 
		drop_na(session_id) |> 
		select(session_id, total_prop, contents)
	
	cdi_extended <- get_cdi_extended_oxf(vocabulary_file) |> 
		left_join(stimuli_cdi_oxf$full, by = c("item" = "item_cdi")) |> 
		mutate(item = if_else(!is.na(stimulus), stimulus, item)) |>  
		rename(vocab_id_response = response_id) |> 
		summarise(total_prop = mean(response),
				  contents = list(item[response]),
				  .by = c(vocab_id_response)) |> 
		left_join(select(participants_tmp, session_id, vocab_id_response),
				  by = join_by(vocab_id_response)) |> 
		drop_na(session_id) |> 
		select(session_id, total_prop, contents)
	
	cdi_supplementary <- get_supplementary_oxf(vocabulary_file) |> 
		left_join(stimuli_cdi_oxf$supp, by = c("item" = "item_cdi")) |> 
		mutate(item = if_else(!is.na(stimulus), stimulus, item)) |>  
		rename(session_id = child_id) |> 
		summarise(total_prop = mean(response, na.rm = TRUE),
				  contents = list(item[response]),
				  .by = c(session_id)) |> 
		select(session_id, total_prop, contents) 
	
	
	vocabulary <- lst(cdi_full, cdi_extended, cdi_supplementary) |> 
		bind_rows(.id = "version") |> 
		arrange(version) |> 
		distinct(session_id, .keep_all = TRUE) |> 
		left_join(participants_tmp,
				  by = join_by(session_id)) |> 
		mutate(is_imputed = FALSE,
			   l1_prop = total_prop,
			   l2_prop = 0,
			   concept_prop = total_prop,
			   te_prop = 0) |> 
		select(child_id, session_id, vocab_id, is_imputed, matches("prop"), contents) |> 
		distinct(child_id, session_id, vocab_id, .keep_all = TRUE)
	
	return(vocabulary)
	
}

#' Get responses from CDI (full version)
get_cdi_full_oxf <- function(vocabulary_file) {
	
	# cdi_full ---------------------------------------------------------------------
	
	category_names <- c("sounds",
						"animals",
						"vehicles",
						"toys",
						"food_and_drink",
						"and_rooms",
						"body_parts",
						"clothes",
						"furniture_bathroom",
						"furniture_and_rooms",
						"outside",
						"household_items",
						"people",
						"games_and_routines",
						"action_words",
						"descriptive_words",
						"question_words",
						"time",
						"pronouns_and_possessives",
						"prepositions_and_location_words",
						"quantifiers")
	
	cdi_full <- readxl::read_xlsx(vocabulary_file, 
								  sheet = "CDI_full",
								  .name_repair = janitor::make_clean_names,
								  na = c("N/A", "NA", "", "?", "none")) |> 
		relocate(unique_id, premature_birth, hearing_problems, comments) |>  
		mutate(across(sounds_baa_baa_sheep:quantifiers_some,
					  recode_responses),
			   across(c(premature_birth, hearing_problems),
			   	   \(x) !grepl("no", tolower(x))),
			   age_in_months = as.numeric(gsub("[^0-9.-]", "", age_in_months))) |> 
		filter(finished) |> 
		select(unique_id, response_id, recorded_date,
			   sounds_baa_baa_sheep:quantifiers_some) |> 
		drop_na(unique_id, response_id) |> 
		pivot_longer(sounds_baa_baa_sheep:quantifiers_some,
					 names_to = "item",
					 values_to = "response") |> 
		mutate(semantic_category = str_extract(item,
											   paste0(rev(category_names), 
											   	   collapse = "|")),
			   item = gsub(paste0(paste0(rev(category_names), "_"),
			   				   collapse = "|"), "", item))
	
	return(cdi_full)
}

#' Get CDI extended
get_cdi_extended_oxf <- function(vocabulary_file) {
	
	category_names_2 <- c("animal_sounds",
						  "animals",
						  "vehicles",
						  "adventures",
						  "toys",
						  "food",
						  "action",
						  "food_and_drink",
						  "body_parts",
						  "clothes",
						  "furniture_and_rooms",
						  "furniture",
						  "outside",
						  "household_items",
						  "household_objects",
						  "people",
						  "food",
						  "games_and_routines",
						  "action_words",
						  "descriptive_words",
						  "question_words",
						  "parts_of_things",
						  "parts_of_animals",
						  "time",
						  "household_objects",
						  "pronouns",
						  "online",
						  "prepositions",
						  "quantifiers")
	
	cdi_ext <- readxl::read_xlsx(vocabulary_file, 
								 sheet = "CDI_ext",
								 .name_repair = janitor::make_clean_names,
								 na = c("N/A", "NA", "", "?", "none")) |> 
		relocate(premature_birth, hearing_problems, comments) |>  
		mutate(across(animal_sounds_baa_baa:online_youtube,
					  recode_responses),
			   across(c(premature_birth, hearing_problems),
			   	   \(x) !grepl("no", tolower(x)))) |> 
		filter(finished) |> 
		select(response_id, recorded_date,
			   animal_sounds_baa_baa:online_youtube) |> 
		drop_na(response_id) |> 
		pivot_longer(animal_sounds_baa_baa:online_youtube,
					 names_to = "item",
					 values_to = "response") |> 
		mutate(semantic_category = str_extract(item,
											   paste0(rev(category_names_2), 
											   	   collapse = "|")),
			   item = gsub(paste0(paste0(rev(category_names_2), "_"),
			   				   collapse = "|"), "", item))
	
	return(cdi_ext)
}

#' Get supplementary vocabulary
get_supplementary_oxf <- function(vocabulary_file) {
	
	supplementary <- readxl::read_xlsx(vocabulary_file, 
									   col_types = "text",
									   sheet = "Supplementary",
									   .name_repair = janitor::make_clean_names,
									   na = c("N/A", "NA", "", "?", "none")) |> 
		drop_na(participant_id) |> 
		mutate(across(-participant_id, recode_responses)) |> 
		pivot_longer(-participant_id,
					 names_to = "item",
					 values_to = "response") |> 
		rename(child_id = participant_id) 
	return(supplementary)
}

#' Recode CDI responses
recode_responses <- function(x) {
	
	suppressWarnings({
		y <- case_when(x %in% c("N", "No") ~ "0",
					   x %in% c("U", "U/S") ~ "1",
					   .default = x)
		y <- as.logical(as.numeric(y))
	})
	
	return(y)
}



