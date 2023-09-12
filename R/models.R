#' Fit multiple models given a list of formulas
get_model_fit <- function(formula, data, ...) {
	
	# check args
	if (is.null(names(formula)) || !is.list(formula)) {
		cli_abort("formula must be a named list")
	}
	if (!is.data.frame(data)) {
		cli_abort('data must be a data frame')
	}
	
	# fit models
	fit <- purrr::map2(.x = formula,
					   .y = names(formula),
					   .f = function(formula, name, data, ...) {
					   	fit_single_model(formula, name, data, ...)
					   },
					   .progress = TRUE)
	
	return(fit)
}

#' Estimate model using Hamiltonian Monte Carlo via Stan
fit_single_model <- function(formula, name, data,...) {
	
	fit <- brms::brm(formula = formula,
					 data = data,
					 iter = 500,
					 chains = 6,
					 cores = 6,
					 init = 0.1,
					 file_refit = "on_change",
					 file = file.path("results", paste0(name, ".rds")),
					 seed = 1234,
					 ...)
	return(fit)
}	

#' Leave-one-out cross-validation (LOO-CV)
get_model_loos <- function(model, ...) {
	
	loos <- brms::loo_compare(purrr::map(.x = model, 
										 .f = \(x) brms::loo(x, ...), 
										 .progress = TRUE))
	
	return(loos)
}