# analysis

fit_models <- function(
	formulas,
	datasets,
	file,
	save_model
){
	
	p <- c(
		prior(normal(0, 1), class = "b"),
		prior(exponential(2), class = "sigma"),
		prior(exponential(2), class = "sd"),
		prior(lkj(2), class = "cor")
	)
	
	formulas <- map(formulas, as.formula)
	
	fits <- map2(
		.x = formulas,
		.y = datasets,
		~brm(
			formula = .x, data = .y, prior = p, backend = "cmdstanr",
			file = file, save_model = save_model,
			init = 0, iter = 2000, chains = 4, seed = 888, cores = 4
		) 
	)
	
	return(fits)
}
                