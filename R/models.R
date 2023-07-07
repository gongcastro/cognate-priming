get_model_fit <- function(formula, data,...) {
	
	fit <- brm(
		formula,
		data = data,
		iter = 1000,
		chains = 4,
		cores = 4,
		file_refit = "on_change",
		...
	)
	
	return(fit)
}
