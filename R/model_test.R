
fit <- brm(
	prop ~ 1 + time_bin_center + 
		s(time_bin_center, bs = "cs", by = trial_type),
	family = Beta(),
	data = gaze,
	# prior = c(
	# 	prior(normal(0, 0.1), class = "Intercept"),
	# 	prior(normal(0, 0.5), class = "b"),
	# 	prior(normal(0, 0.5), class = "sds"),
	# 	prior(normal(5, 0.1), class = "phi")
	# 	# prior(exponential(7), class = "sd"),
	# 	# prior(lkj(8), class = "cor")
	# ),
	iter = 500,
	chains = 6,
	cores = 6
)


post <- gather_draws(
	fit,
	`b_.*`,
	`sds_.*`,
	regex = TRUE
) %>% 
	mutate(
		.chain = as.factor(.chain),
		group = case_when(
			str_detect(.variable, "b_") ~ "b",
			str_detect(.variable, "sds_") ~ "sds",
			str_detect(.variable, "sd_|cor_") ~ "sd"
		)
	)

post %>% 
	ggplot() +
	aes(
		x = .value,
		y = .variable
	) +
	facet_wrap(~group, scales = "free_y", ncol  =1) + 
	geom_vline(xintercept = 0, linetype = "dotted") +
	stat_pointinterval() +
	theme_custom()

nd <- expand.grid(
	time_bin_center = seq(min(gaze$time_bin_center), max(gaze$time_bin_center), 0.1),
	trial_type = unique(gaze$trial_type)
)

preds <- add_epred_draws(
	nd,
	fit,
	ndraws = 50,
	re_formula = NA
)

preds %>% 
	ggplot() +
	aes(
		x = time_bin_center,
		y = .epred,
		colour = trial_type,
		fill = trial_type
	) +
	# geom_line(
	# 	aes(group = interaction(.draw, trial_type)),
	# 	alpha = 0.2,
	# 	size = 0.75
	# ) +
	stat_summary(
		fun = mean,
		geom = "line",
		size = 1
	) +
	stat_summary(
		fun.data = mean_qi,
		geom = "ribbon",
		colour = NA,
		alpha = 0.5
	)


nd_re <- expand.grid(
	time_bin_center = seq(min(gaze$time_bin_center), max(gaze$time_bin_center), 0.1),
	trial_type = unique(gaze$trial_type),
	participant = unique(fit$data$participant)[1:20]
)

preds_re <- add_epred_draws(
	nd_re,
	fit,
	ndraws = 50
)

preds_re %>% 
	ggplot() +
	aes(
		x = time_bin_center,
		y = .epred,
		colour = trial_type,
		fill = trial_type,
	) + 
	facet_wrap(~participant) +
	# geom_line(
	# 	aes(group = interaction(.draw, trial_type)),
	# 	alpha = 0.2,
	# 	size = 0.75
	# ) +
	stat_summary(
		fun.data = mean_qi,
		geom = "ribbon",
		colour = NA,
		alpha = 0.25
	) +
	stat_summary(
		fun = mean,
		geom = "line",
		size = 1
	) +

	scale_color_d3() +
	scale_fill_d3()












