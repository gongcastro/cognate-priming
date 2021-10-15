# analysis

fit_models <- function(
	formulas,
	datasets,
	file,
	save_model
){
	
	p <- c(
		prior(normal(0, 0.1), class = "b"),
		prior(exponential(6), class = "sigma"),
		prior(exponential(6), class = "sd"),
		prior(lkj(8), class = "cor")
	)
	
	formulas <- map(formulas, as.formula)
	
	fits <- map2(
		.x = formulas,
		.y = datasets,
		~brm(
			formula = .x, data = .y, prior = p, backend = "cmdstanr",
			file = file, save_model = save_model,
			init = 0, iter = 500, chains = 3, seed = 888, cores = 3
		) 
	)
	
	return(fits)
}
