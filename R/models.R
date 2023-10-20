#' Fit multiple models given a list of formulas
get_model_fit <- function(names, formulas, data, family, prior, ...) {
	
	# check args
	if (is.null(names(formulas)) || !is.list(formulas)) {
		cli_abort("formula must be a named list")
	}
	if (!is.data.frame(data)) {
		cli_abort('data must be a data frame')
	}
	
	# fit models
	fit <- purrr::map2(.x = names,
					   .y = formulas,
					   .f = \(names, formulas) {
					   	fit_single_model(names, formulas, data, family, prior, ...)
					   },
					   .progress = TRUE)
	
	return(fit)
}

#' Estimate model using Hamiltonian Monte Carlo via Stan
fit_single_model <- function(name, formula, data, family, prior, ...) {
	
	fit <- brms::brm(formula, data, family, prior,
					 iter = 500,
					 chains = 6,
					 cores = 6,
					 init = 0.1,
					 file_refit = "on_change",
					 file = file.path("results", "fits", paste0(name, ".rds")),
					 seed = 1234,
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