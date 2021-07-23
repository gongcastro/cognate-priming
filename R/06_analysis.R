# analysis

fit_models <- function(
	formulas,
	datasets,
	names = NULL,
	control = lmerControl(optimizer = "bobyqa")
){
	formulas <- map(formulas, as.formula)
	fits <- map2(
		.x = formulas,
		.y = datasets,
		~lmer(
			formula = .x,
			data = .y,
			control = control
		)
	)
	
	if (!is.null(formulas)){
		fits <- set_names(fits, names(formulas))
	} else if (!is.null(datasets)){
		fits <- set_names(fits, names(datasets))
	}
	
	return(fits)
}
