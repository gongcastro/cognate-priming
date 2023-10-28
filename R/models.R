#' Fit multiple models given a list of formulas
get_model_fit <- function(names, formulas, data, prior, ...) {
	
	# check args
	if (!is.list(formulas)) {
		cli_abort("formula must be a named list")
	}
	if (!is.data.frame(data)) {
		cli_abort('data must be a data frame')
	}
	
	# fit models
	fit_lst <- map2(.x = names,
					.y = formulas,
					.f = \(names, formulas) {
						fit_single_model(names, formulas, data, prior, ...)
					},
					.progress = TRUE) |> 
		set_names(gsub("cognate_|related_", "", names))
	
	return(fit_lst)
}

#' Estimate model using Hamiltonian Monte Carlo via Stan
fit_single_model <- function(name, formula, data, prior, ...) {
	
	model_path <- file.path("results", "fits", paste0(name, ".rds"))
	
	fit <- brms::brm(formula = formula,
					 data = data,
					 prior = prior,
					 iter = 500,
					 chains = 6,
					 cores = 6,
					 init = 0.1,
					 file_refit = "on_change",
					 file = model_path,
					 seed = 1234,
					 control = list(adapt_delta = 0.9,
					 			   max_treedepth = 15),
					 silent = 2,
					 ...)
	
	return(fit)
}	

#' Leave-one-out cross-validation (LOO-CV)
get_model_loos <- function(models, ...) {
	
	loos <- brms::loo_compare(purrr::map(.x = models, 
										 .f = \(x) brms::loo(x, ...), 
										 .progress = TRUE))
	
	return(loos)
}