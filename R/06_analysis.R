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

# get posterior draws for population-level effects
get_posterior_draws <- function(fit){
	post <- gather_draws(fit, `b_.*`, regex = TRUE)
	return(post)
}

# get expected predictions
get_epreds <- function(fit, gaze, transform = TRUE){
	n <- expand_grid(
		lp = c("Monolingual", "Bilingual"),
		age_group = paste(c(21, 25, 30), "months"),
		trial_type = unique(gaze$trial_type),
		time_bin_center = seq(min(gaze$time_bin_center), max(gaze$time_bin_center), 0.5)
	)
	
	m <- epred_draws(object = fit, newdata = n, ndraws = 50, re_formula = NA)
	
	if (transform) m$.epred <- logit_to_prob(m$.epred)

	return(m)
}


# get marginal means
get_emmeans <- function(fit, ...){
	
	emmean <- emmeans(fit, ~trial_type, epred = TRUE, ...) %>% 
		as_tibble() %>% 
		mutate_if(is.numeric, inv_logit_scaled)
	
	return(emmean)
}

