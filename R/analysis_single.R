# fit data from single participant

tar_load_globals()
tar_load(model_datasets)

library(brms)
library(tidybayes)
library(ggplot2)

g <- model_datasets$fit %>% 
	mutate(time_bin_center = scale(time_bin, scale = FALSE)[,1],
		   vocab_size_l1_center = scale(vocab_size_l1_center)[,1])

p <- c(
	prior(normal(0, 0.1), class = "b"),
	prior(exponential(6), class = "sigma"),
	prior(exponential(6), class = "sd"),
	prior(lkj(8), class = "cor")
)

f <- brm(
	logit_adjusted ~ (time_bin_center + I(time_bin_center^2) + I(time_bin_center^3))*trial_type*lp + age_group +
		(1 + time_bin_center*trial_type + age_group | participant),
	data = g, prior = p, backend = "cmdstanr", init = 0,
	iter = 2000, chains = 4, seed = 888, cores = 4
) 

n <- expand_grid(
	lp = c("Monolingual", "Bilingual"),
	age_group = c("25 months"),
	trial_type = unique(g$trial_type),
	time_bin_center = seq(min(g$time_bin_center), max(g$time_bin_center), 0.5)
)

n_p <- distinct(g, participant, lp, age_group) %>% 
	left_join(
		expand_grid(
			lp = c("Monolingual", "Bilingual"),
			age_group = c("21 months", "25 months", "30 months"),
			trial_type = unique(g$trial_type),
			time_bin_center = seq(min(g$time_bin_center), max(g$time_bin_center), 0.5)
		)
	)
m <- epred_draws(object = f, newdata = n, ndraws = 20, re_formula = NA) %>% 
	mutate(.value = logit_to_prob(.epred))

m_p <- epred_draws(object = f, newdata = n_p, ndraws = 20) %>% 
	mutate(.value = logit_to_prob(.epred))

# by participants
ggplot(m, aes(time_bin_center, .value, colour = trial_type, fill = trial_type, shape = trial_type)) +
	facet_wrap(~lp) +
	geom_hline(yintercept = 0.5) +
	geom_line(aes(group = interaction(age_group, trial_type, lp, .draw)), alpha = 0.15, size = 0.75) +
	stat_summary(aes(group = trial_type), fun = mean, geom = "line", size = 1.25) +
	stat_summary(data = g, aes(y = logit_to_prob(logit_adjusted), group = trial_type),
				 fun.data = mean_se, geom = "errorbar", size = 0.5,
				 position = position_dodge(width = 0.3)) +
	labs(x = "Time bin (100 ms)", y = "Expected posterior mean PTLT", colour = "Trial type",
		 fill = "Trial type", shape = "Trial type", linetype = "Trial type") +
	scale_color_brewer(palette = "Set1") +
	scale_x_continuous(breaks = min(g$time_bin_center):max(g$time_bin_center), labels = unique(g$time_bin)) +
	scale_y_continuous(limits = c(0, 1), labels = percent) + 
	theme_custom() +
	theme(
		legend.position = "top",
		legend.title = element_blank(),
		axis.text.x = element_text(size = 9)
	)

# by participants
ggplot(m_p, aes(time_bin_center, .value, fill = age_group, colour = age_group, linetype = trial_type)) +
	facet_wrap(~participant) +
	geom_hline(yintercept = 0.5) +
	# geom_line(aes(group = interaction(age_group, trial_type, .draw)), alpha = 0.5, size = 0.5) +
	stat_summary(aes(group = interaction(participant, trial_type, age_group)), fun = mean, geom = "line") +
	# stat_summary(data = g, aes(y = logit_to_prob(logit_adjusted), group = trial_type),
	# 			 fun.data = mean_se, geom = "pointrange", size = 0.35,
	# 			 position = position_dodge(width = 0.25)) +
	labs(x = "Time bin (100 ms)", y = "Expected posterior mean PTLT", colour = "Age group",
		 fill = "Age group", shape = "Age group", linetype = "Trial type") +
	scale_color_brewer(palette = "Set1") +
	scale_x_continuous(breaks = min(g$time_bin_center):max(g$time_bin_center), labels = unique(g$time_bin)) +
	scale_y_continuous(limits = c(0, 1), labels = percent) + 
	theme_custom() +
	theme(
		legend.position = "right",
		legend.title = element_blank(),
		axis.text.x = element_text(size = 8)
	)

