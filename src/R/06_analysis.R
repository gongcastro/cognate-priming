# analysis

fit_models <- function(
	formulas,
	datasets,
	prior,
	model_paths,
	refit = TRUE
){
	
	fit_path <- here("results", "model_fits.rds")
	fit_exists <- file.exists(fit_path)
	
	if (!fit_exists){
		formulas <- map(formulas, as.formula)
		
		fits <- map2(
			.x = formulas, .y = datasets,
			~brm(
				formula = .x, data = .y, prior = prior, backend = "cmdstanr", 
				init = 0, iter = 2000, chains = 4, seed = 888, cores = 4
			) 
		)
		
		names(fits) <- names(formulas)
		
		# save file
		saveRDS(fits, fit_path)
		
		# save Stan code
		stan_code <- map(fits, stancode)
		map2(.x = stan_code, .y = model_paths, ~save(.x, .y))
		
	} else {
		fit <- readRDS(fit_path)
	}
	
	return(fit)
}

# get posterior draws for population-level effects
get_posterior_draws <- function(fit){
	post <- map(fit, ~gather_draws(., `b_.*`, regex = TRUE))
	names(post) <- names(fit)
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
	
	m <- map(fit, function(x){
		y <- epred_draws(object = x, newdata = n, ndraws = 50, re_formula = NA)
		if (transform) y$.epred <- logit_to_prob(y$.epred)
		return(y)
	})
	
	names(m) <- names(fit)
	
	return(m)
}


# get marginal means
get_emmeans <- function(fit, ...){
	
	emmean <- map(fit, function(x){
		y <- emmeans(x, ~trial_type, epred = TRUE, ...) %>% 
			as_tibble() %>% 
			mutate_if(is.numeric, inv_logit_scaled)
		
		return(y)
	})
	names(emmean) <- names(fit)
	return(emmean)
}
