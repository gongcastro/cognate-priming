# vocabulary size
get_vocabulary_bcn <- function(participants, # participants dataset (get_participants output)
							   bvq_data
){
	
	# vocabulary sizes BVQ database and Cognate Priming
	participants_tmp <- select(participants, id, id_vocab,
							   age_group, age, lp, filename) |> 
		mutate(id_vocab = if_else(is.na(id_vocab),
								  paste0(id, "_na"),
								  id_vocab))
	
	# get vocabulary contents
	
	vocabulary <- bvq_data$vocabulary |>
		rename(id = child_id,
			   id_vocab = response_id) |> 
		right_join(select(participants_tmp, id, id_vocab),
				   by = join_by(id, id_vocab)) |> 
		rowwise() |> 
		mutate(is_imputed = any(is.na(c_across(total_prop:te_prop)))) |> 
		ungroup() |> 
		relocate(id, id_vocab, is_imputed) |> 
		full_join(select(participants_tmp, id, id_vocab, age),
				  by = join_by(id, id_vocab),
				  na_matches = c("never"),
		) 
	# multiple imputation
	impute_vocabulary(cols_impute = c("total_prop", "l1_prop",
									  "l2_prop", "concept_prop",
									  "te_prop"), 
					  cols_predictor = c("age", "lp"), 
					  bvq_data = bvq_data) |> 
		# merge both datasets
		right_join(participants_tmp, imputed,
				   by = join_by(id_db, age_group, lp)) |> 
		mutate(age_group = factor(age_group,
								  levels = paste0(c(21, 25, 30), " months")),
			   lp = factor(lp, levels = c("Monolingual", "Bilingual"))) |> 
		left_join(vocab_contents, by = join_by(id_db, age_group, lp)) |> 
		select(filename, is_imputed, matches("prop|count|contents")) |> 
		distinct(filename, .keep_all = TRUE)
	
	test_vocabulary(vocabulary)
	
	return(vocabulary)
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
					 by = join_by(id, id_exp, age_group))
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

#' Get vocabulary contents
get_vocabulary_contents <- function(participants,
									bvq_data) {
	
	# total vocabulary contents
	contents_total <- bvq_data$responses |> 
		dplyr::filter(response > 1) |> 
		summarise(total_contents = list(item[response > 1]),
				  .by = c(id, time, code)) 
	
	# L1 and L2 vocabulary contents
	contents_dominance <- bvq_data$responses |> 
		dplyr::filter(response > 1) |> 
		mutate(language = ifelse(grepl("^cat_", item), "Catalan", "Spanish"),
			   dominance = ifelse(doe_catalan >= doe_spanish, "Catalan", "Spanish"),
			   item_dominance = ifelse(language==dominance, "L1", "L2")) |> 
		summarise(contents = list(item[response > 1]),
				  .by = c(id, time, code, item_dominance)) |> 
		pivot_wider(names_from = item_dominance,
					values_from = contents,
					names_repair = janitor::make_clean_names,
					names_glue = "{item_dominance}_contents") 
	
	# conceptual vocabulary contents
	contents_concept <- bvq_data$responses |> 
		dplyr::filter(response > 1) |> 
		left_join(select(bvq::pool, te, item),
				  relationship = "many-to-many",
				  by = join_by(item)) |> 
		distinct(id, time, code, te) |> 
		summarise(concept_contents = list(te),
				  .by = c(id, time, code))
	
	# translation equivalent contents
	contents_te <- bvq_data$responses |> 
		dplyr::filter(response > 1) |> 
		left_join(select(bvq::pool, te, item),
				  relationship = "many-to-many",
				  by = join_by(item)) |> 
		count(id, time, code, te) |> 
		summarise(te_contents = list(te[n > 1]),
				  .by = c(id, time, code))
	
	# join datasets
	logs_tmp <- bvq::bvq_logs(bvq_data$participants,
							  bvq_data$responses) |> 
		mutate(age_group = case_when(age>=19 & age<24 ~ "21 months",
									 age>=24 & age<28 ~ "25 months",
									 age>=28 & age<=34 ~ "30 months",
									 TRUE ~ "Other"),
			   age_group = as.factor(age_group)) |> 
		select(id, time, age_group)
	
	participants_tmp <- select(participants, id = id_db, age_group, lp) |>
		left_join(logs_tmp, by = join_by(id, age_group))
	
	contents <- list(contents_total,
					 contents_dominance,
					 contents_concept,
					 contents_te) |> 
		purrr::reduce(full_join, by = join_by(id, time, code)) |> 
		right_join(participants_tmp, by = join_by(id, time)) |> 
		rename(id_db = id)
	
	return(contents)
	
}


# Oxford functions -------------------------------------------------------------

#' Import and process vocabulary data
get_vocabulary_oxf <- function(vocabulary_file, participants) {
	
	participants_tmp <- participants |> 
		filter(location=="Oxford")
	select(id, id_vocab) |> 
		drop_na()
	
	cdi_full <- get_cdi_full_oxf(vocabulary_file)
	cdi_extended <- get_cdi_extended_oxf(vocabulary_file)
	cdi_supplementary <- get_supplementary_oxf(vocabulary_file)
	
	vocabulary_tmp <- lst(cdi_full, cdi_extended) |> 
		bind_rows(.id = "version") |> 
		relocate(unique_id, response_id, semantic_category, item, response) |> 
		rename(id_vocab = unique_id,
			   id_vocab_response = response_id) |> 
		summarise(n_total = n(),
				  total_prop = mean(response, na.rm = TRUE),
				  total_count = sum(response, na.rm = TRUE),
				  vocab_contents = list(item[response]),
				  .by = c(id_vocab, id_vocab_response)) 
	
	vocabulary_id <- vocabulary_tmp |> 
		select(-id_vocab_response) |>
		distinct(id_vocab, .keep_all = TRUE) |> 
		inner_join(select(participants_tmp, id, id_vocab),
				   by = join_by(id_vocab)) |>  
		select(-id_vocab)
	
	vocabulary_id_response <- vocabulary_tmp |> 
		distinct(id_vocab_response, .keep_all = TRUE) |> 
		inner_join(select(participants_tmp, id, id_vocab_response),
				   by = join_by(id_vocab_response)) |>  
		select(-c(id_vocab, id_vocab_response))
	
	vocabulary <- cdi_supplementary |> 
		left_join(vocabulary_id, by = join_by(id)) |> 
		left_join(vocabulary_id_response,
				  by = join_by(id, n_total, total_prop, total_count,
				  			 vocab_contents)) |> 
		relocate(id)
	
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
						"body_parts",
						"clothes",
						"furniture",
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
											   paste0(category_names, 
											   	   collapse = "|")),
			   item = gsub(paste0(paste0(category_names, "_"),
			   				   collapse = "|"), "", item))
	
	return(cdi_full)
}

#' Get CDI extended
get_cdi_extended_oxf <- function(vocabulary_file) {
	
	category_names_2 <- c("animal_sounds",
						  "animals",
						  "vehicles",
						  "toys",
						  "food_and_drink",
						  "body_parts",
						  "clothes",
						  "furniture_and_rooms",
						  "outside",
						  "household_items",
						  "people",
						  "games_and_routines",
						  "action_words",
						  "action",
						  "descriptive_words",
						  "question_words",
						  "time",
						  "pronouns",
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
											   paste0(category_names_2, 
											   	   collapse = "|")),
			   item = gsub(paste0(paste0(category_names_2, "_"),
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
		rename(id = participant_id) |> 
		summarise(vocab_contents_supp = list(item[response]),
				  vocab_sum_supp = sum(response, na.rm = TRUE),
				  vocab_n_supp = n(), 
				  .by = id)
	
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



