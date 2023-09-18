# vocabulary size
get_vocabulary <- function(participants, # participants dataset (get_participants output)
						   bvq_data
){
	
	# vocabulary sizes BVQ database and Cognate Priming
	participants_tmp <- subset(participants, select = c(id_db, age_group, age, lp, filename))
	
	# get vocabulary contents
	vocab_contents <- get_vocabulary_contents(participants, bvq_data)
	
	vocabulary <- bvq_data$vocabulary_comp |>
		mutate(id_db = id) |> 
		right_join(select(participants_tmp, id_db, age_group, lp),
				   by = join_by(id_db, age_group)) |> 
		rowwise() |> 
		mutate(is_imputed = any(is.na(c_across(total_prop:te_prop)))) |> 
		ungroup() |> 
		relocate(id, age_group, is_imputed) |> 
		left_join(select(participants_tmp, id_db, age_group, age),
				  by = join_by(age_group, id_db)) |> 
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
	db <- inner_join(bvq_data$vocabulary_comp, 
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
