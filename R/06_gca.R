# 04_gca: Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Pompeu Fabra University

#### set up ################################################

# load packages
library(tibble)      # for more informative data sets
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(magrittr)
library(data.table)  # for importing data
library(ggplot2)     # for visualising data
library(janitor)
library(stringr)     # for manipulating character strings
library(lme4)        # for fitting frequentist mixed effect models
library(ggeffects)   # for predicting means
library(lmerTest)         # for Wald test
library(purrr)       # for extracting elements from lists
library(forcats)     # for dealing with categorical variables
library(patchwork)   # for arranging plots
library(here)        # for locating files

# set params
time_bin_duration <- 100

#### import data ###########################################
dat <- fread(here("Data", "04_prepared.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
	as_tibble() %>%
	distinct(participant_id, trial_id, timebin, .keep_all = TRUE)

#### fit model #############################################

# maximal model with crossed-random effeto for participant and trial
fit0 <- glmer(cbind(target, distractor) ~
			  	(timebin1 + timebin2 + timebin3) * lang_profile  +
			  	(timebin1 + timebin2 + timebin3 | participant_id),
			  na.action = na.exclude,
			  family = binomial(link = "logit"),
			  data = dat)
fit1 <- glmer(cbind(target, distractor) ~
				(timebin1 + timebin2 + timebin3) * trial_type * lang_profile  +
				(timebin1 + timebin2 + timebin3 | participant_id:trial_type),
			na.action = na.exclude,
			family = binomial(link = "logit"),
			data = dat)

summary(fit0)               # extract summary of model
summary(fit1)

#### coefficients ##########################################
coefs <- summary(fit) %$% 
	coefficients %>%
	as.data.frame() %>%
	clean_names() %>%
	rownames_to_column("term") %>%
	mutate(term = str_remove_all(term, "\\(|\\)"))

#### visualise results #####################################
dat %>%
	filter(timebin > 2) %>%
	mutate(lang_profile = factor(lang_profile, levels = c(-0.5, 0.5), labels = c("Monolinguals", "Bilinguals")),
		   language = factor(language, levels = c(-0.5, 0.5), labels = c("Monolinguals", "Bilinguals")),
		   trial_type = factor(trial_type, levels = c(0, 1, 2), labels = c("Unrelated", "Non-cognate", "Cognate"))) %>%
	rowwise() %>%
	mutate(prop = target/total) %>%
	ggplot(aes(x = time, y = prop, colour = trial_type, fill = trial_type)) +
	stat_summary(fun.data = "mean_se", geom = "pointrange", size = 0.25) +
	stat_summary(aes(y = fitted(fit0)), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
	stat_summary(aes(y = fitted(fit0)), fun = "mean", geom = "line") +
	labs(title = "Null model", x = "Time (ms)", y = "Proportion of fixations",
		 colour = "Trial type", fill = "Trial type") +
	scale_x_continuous(breaks = seq(200, 1200, by = 100)) +
	
	dat %>%
	filter(timebin > 2) %>%
 	mutate(lang_profile = factor(lang_profile, levels = c(-0.5, 0.5), labels = c("Monolinguals", "Bilinguals")),
		   language = factor(language, levels = c(-0.5, 0.5), labels = c("Monolinguals", "Bilinguals")),
		   trial_type = factor(trial_type, levels = c(0, 1, 2), labels = c("Unrelated", "Non-cognate", "Cognate"))) %>%
	rowwise() %>%
	mutate(prop = target/total) %>%
	ggplot(aes(x = time, y = prop, colour = trial_type, fill = trial_type)) +
	facet_wrap(~lang_profile) +
	stat_summary(fun.data = "mean_se", geom = "pointrange", size = 0.25) +
	stat_summary(aes(y = fitted(fit1)), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
	stat_summary(aes(y = fitted(fit1)), fun = "mean", geom = "line") +
	labs(title = "Extended model", x = "Time (ms)", y = "Proportion of fixations",
		 colour = "Trial type", fill = "Trial type") +
	scale_x_continuous(breaks = seq(200, 1200, by = 100)) +
	plot_layout(guides = "collect", ncol = 2, widths = c(1/3, 2/3)) &
	plot_annotation(tag_levels = "A",
					title = "Model predictions",
					subtitle = "Lines represent mean predictions and shaded ribbons represent +1/-1 SEM") &
	theme_grey() &
	theme(legend.position = "top",
		  axis.text = element_text(colour = "black")) &
	
	ggsave(here("Figures", "06_gca-predictions.png"), height = 4, width = 8)
