# dat <- df |> 
# 	select(id, age, time_bin, ot1, ot2, ot3, lp, trial_type, prop) 
# 
# 
# fit <- brm(data = dat,
# 		   bf(prop ~ s(time_bin, bs = "cr", by = interaction(lp, trial_type)) +
# 		      	s(age, bs = "cr", by = interaction(lp, trial_type)) +
# 		      	s(id, bs = "re"),
# 		      phi ~ s(time_bin, bs = "cr", by = interaction(lp, trial_type)) + 
# 		      	s(age, bs = "cr", by = interaction(lp, trial_type)) +
# 		      	s(id, bs = "re")),
# 		   family = Beta,
# 		   prior = c(prior(normal(0, 0.5), class = Intercept),
# 		   		  prior(normal(0, 1), class = b),
# 		   		  prior(exponential(4), class = sds),
# 		   		  prior(normal(0, 0.5), class = Intercept, dpar = phi),
# 		   		  prior(normal(0, 1), class = b, dpar = phi),
# 		   		  prior(exponential(4), class = sds, dpar = phi)),
# 		   iter = 2000,
# 		   thin = 10,
# 		   file = "results/test.rds",
# 		   file_refit = "on_change",
# 		   warmup = 1000,
# 		   init = 1,
# 		   chains = 2,
# 		   cores = 4,
# 		   seed = 888)
# 
# expand_grid(distinct(fit$data, lp, trial_type),
# 			time_bin = seq(min(fit$data$time_bin),
# 						   max(fit$data$time_bin),
# 						   length.out = 100)) |> 
# 	add_epred_draws(fit, ndraws = 20, re_formula = NA) |> 
# 	ggplot(aes(x = time_bin, 
# 			   y = prop,
# 			   colour = trial_type)) +
# 	facet_wrap(lp~id) +
# 	# geom_smooth() +
# 	geom_hline(yintercept = plogis(fixef(fit)[1, 1]), linetype = 2) +
# 	# geom_point(alpha = 1/5, shape = 1, stroke = 1) +
# 	# geom_ribbon(alpha = 1/5, colour = NA) +
# 	geom_line(aes(y = .epred, group = interaction(trial_type, .draw)),
# 			  linewidth = 0.75,
# 			  alpha = 1/5) +
# 	labs(subtitle = "b4.7 using s(year)",
# 		 y = "day in year") +
# 	theme_ggdist() +
# 	theme(panel.grid = element_blank())
