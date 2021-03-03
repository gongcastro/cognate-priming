#### analysis: Analyse data ----------------------------------------------------

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(brms)
library(tidybayes)
library(here)

# load functions
source(here("R", "utils.R"))

# set params
options(mc.cores = 4, seed = 888)

#### import data ---------------------------------------------------------------
attrition <- readRDS(here("Results", "attrition.rds"))
gaze_trial <- readRDS(here("Results", "gaze_trial.rds")) %>% 
	filter(valid_trial, valid_participant) %>% 
	mutate(proportion = fixations/n) %>% 
	select(-matches("valid")) %>% 
	mutate_at(vars(lp, trial_type), as.factor) %>% 
	mutate(vocab_size = scale(vocab_size)[,1])

contrasts(gaze_trial$lp) <- c(-0.5, 0.5)
contrasts(gaze_trial$trial_type) <- cbind(c(-0.5, -0.5, 1), c(0.5, -0.5, 0))

gaze_time <- readRDS(here("Results", "gaze_time.rds")) %>% 
	filter(valid_trial, valid_participant) %>% 
	mutate(time_bin1 = poly(time_bin, 3, simple = TRUE)[,1],
		   time_bin2 = poly(time_bin, 3, simple = TRUE)[,2],
		   time_bin3 = poly(time_bin, 3, simple = TRUE)[,3]) %>%
	mutate_at(vars(lp, trial_type), as.factor)

contrasts(gaze_time$lp) <- c(-0.5, 0.5)
contrasts(gaze_time$trial_type) <- cbind(c(-0.5, -0.5, 1), c(0.5, -0.5, 0))
contrasts(gaze_time$age_group) <- contr.poly(3, contrasts = TRUE)


#### trial-wise ----------------------------------------------------------------
fit_trial <- brm(
	fixations | trials(n) ~ trial_type*lp*age_group + vocab_size + (1 + trial_type | participant),
	data = gaze_trial,
	family = binomial("logit"),
	prior = c(
		prior(normal(0, 5), class = b),
		prior(exponential(4), class = sd),
		prior(lkj(4), class = cor)
	),
	seed = 888,
	save_model = here("Stan", "fit_trials.stan"),
	save_pars = save_pars("all"),
	file = here("Results", "fit_trials.rds")
)

# examine posterior
post_trial <- gather_draws(fit_trial, `b_.*`, `sd_.*`, regex = TRUE)

ggplot(post_trial, aes(.value)) +
	facet_wrap(~.variable, scales = "free") +
	geom_vline(xintercept = 0, linetype = "dashed") +
	stat_halfeye(aes(fill = stat(cut_cdf_qi(
		cdf, 
		.width = c(.5, .8, .95),
		labels = scales::percent_format()
	)))) +
	labs(x = "Value", y = "Variable", fill = "CrI") +
	scale_fill_brewer(direction = -1, na.translate = FALSE) +
	scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
	theme_custom() +
	theme(
		axis.title = element_blank(),
		axis.text = element_text(size = 10),
		panel.grid.major.x = element_line(linetype = "dotted", colour = "grey")
	)

# examine posterior marginal means
post_means <- expand_grid(
	n = 1,
	age_group = as.factor(c(21, 25, 30)),
	lp = c("Monolingual", "Bilingual"),
	trial_type = c("Cognate", "Non-cognate", "Unrelated"),
	vocab_size = c(mean(gaze_trial$vocab_size, na.rm = TRUE))) %>% 
	add_fitted_draws(fit_trial, n = 500, re_formula = NA)

ggplot(post_means, aes(.value, colour = trial_type, fill = trial_type)) +
	facet_grid(lp~age_group) +
	geom_vline(xintercept = 0.5, linetype = "dashed") +
	stat_slab(alpha = 0.5) +
	geom_jitter(data = gaze_trial %>%
					group_by(participant, lp, trial_type, age_group) %>%
					summarise(.value = mean(proportion), .groups = "drop"),
				aes(y = 0.9), shape = 1, stroke = 1, alpha = 0.5, height = 0.05) +
	geom_boxplot(aes(y = 0.7), width = 0.05, position = position_identity(), fill = "white", outlier.colour = NA) +
	labs(x = "Age group (months)", y = "Pr(fixation)", colour = "Condition", fill = "Condition") +
	scale_colour_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	scale_x_continuous(limits = c(0, 1), labels = scales::percent) +
	theme_custom() +
	theme(
		panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
		legend.title = element_blank(),
		legend.position = "top"
	) +
	ggsave(here("Figures", "fit_trials.png"))

#### time bin-wise -------------------------------------------------------------
fit_time <- brm(
	fixations | trials(n) ~ (time_bin1 + time_bin2 + time_bin3)*trial_type*lp*age_group + vocab_size + (1 + (time_bin1 + time_bin2 + time_bin3)*trial_type | participant),
	data = gaze_time,
	family = binomial("logit"),
	prior = c(
		prior(normal(0, 5), class = b),
		prior(exponential(4), class = sd),
		prior(lkj(4), class = cor)
	),
	seed = 888,
	save_model = here("Stan", "fit_time.stan"),
	save_pars = save_pars("all"),
	file = here("Results", "fit_time.rds")
)
