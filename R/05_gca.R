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
library(MASS)
library(papaja)
library(purrr)       # for extracting elements from lists
library(forcats)     # for dealing with categorical variables
library(patchwork)   # for arranging plots
library(here)        # for locating files

# set params
time_bin_duration <- 100

#### import data ###########################################
dat <- fread(here("Data", "04_prepared.csv"), sep = ",", header = TRUE) %>%
	as_tibble() %>%
	mutate(trial_type = factor(trial_type, levels = c("Non-cognate", "Cognate", "Unrelated")),
		   lp = factor(lp, levels = c("Monolingual", "Bilingual")),
		   vocab_comp = scale(vocab_comp)[,1])

#### descriptive stats  ####################################
descriptives <- dat %>%
	group_by(participant_id, lp, trial_type, vocab_comp) %>%
	summarise(prop = mean(prop, na.rm = TRUE), .groups = "drop") %>%
	group_by(lp, trial_type) %>%
	summarise(n = n(),
			  mean_prop = mean(prop, na.rm = TRUE),
			  sd_prop = sd(prop, na.rm = TRUE),
			  mean_vocab_comp = mean(vocab_comp, na.rm = TRUE),
			  sd_vocab_comp = sd(vocab_comp, na.rm = TRUE),
			  .groups = "keep") 
#### descriptive stats  ####################################
descriptive_sample <- dat %>%
	group_by(participant_id, lp, test_language, sex) %>%
	summarise(prop = mean(prop, na.rm = TRUE), .groups = "drop") %>%
	group_by(lp, sex, test_language) %>%
	summarise(n = n(), .groups = "drop") 

#### code contrasts ########################################
contrasts(dat$trial_type) <- contr.helmert(3)/2
contrasts(dat$lp) <- contr.sdif(2)

#### fit model #############################################

# maximal model with crossed-random effeto for participant and trial
fit0 <- glmer(cbind(target, distractor) ~
			  	(timebin1+timebin2+timebin3)*lp  + vocab_comp +
			  	(timebin1+timebin2+timebin3 | participant_id),
			  na.action = na.exclude,
			  family = binomial(link = "logit"),
			  contrasts = list(lp = contrasts(dat$lp)),
			  control = glmerControl(optimizer = "bobyqa"),
			  data = dat)
fit1 <- glmer(cbind(target, distractor) ~
				(timebin1+timebin2+timebin3)*trial_type*lp + vocab_comp +
				(timebin1+timebin2+timebin3 | participant_id:trial_type),
			na.action = na.exclude,
			family = binomial(link = "logit"),
			contrasts = list(lp = contrasts(dat$lp),
							 trial_type = contrasts(dat$trial_type)),
			control = glmerControl(optimizer = "bobyqa"),
			data = dat)

#### compare models ########################################
summary(fit0)               # extract summary of model
summary(fit1)
anova <- anova(fit0, fit1)
#### coefficients ##########################################
confints <- confint.merMod(fit1, method = "Wald") %>%
	as.data.frame() %>% 
	rownames_to_column("term") %>%
	clean_names()
coefs <- summary(fit1) %$% 
	coefficients %>%
	as.data.frame() %>%
	clean_names() %>%
	rownames_to_column("term") %>%
	mutate(term = str_remove_all(term, "\\(|\\)")) %>%
	left_join(confints)


#### export results ########################################
saveRDS(fit0, here("Results", "fit0.rds"))
saveRDS(fit1, here("Results", "fit1.rds"))
fwrite(descriptives, here("Results", "descriptives.csv"), sep = ",", dec = ".", row.names = FALSE)

#### visualise results #####################################
dat %>%
	rowwise() %>%
	mutate(prop = target/total) %>%
	ggplot(aes(x = time, y = prop, colour = trial_type, fill = trial_type)) +
	facet_wrap(~lp) +
	stat_summary(fun = "mean", geom = "point", size = 0.5, alpha = 0.5) +
	stat_summary(aes(y = fitted(fit1)), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
	stat_summary(aes(y = fitted(fit1)), fun = "mean", geom = "line", size = 0.75) +
	labs(x = "Time (ms)", y = "Target / Total fixations",
		 caption = "Lines represent mean predictions and shaded ribbons represent +1/-1 SEM",
		 colour = "Prime", fill = "Prime") +
	scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
	scale_x_continuous(breaks = seq(0, 2000, by = 250)) +
	scale_colour_manual(values = c("#ED7D33", "#5AB152", "#47B1F0")) +
	scale_fill_manual(values = c("#ED7D33", "#5AB152", "#47B1F0")) +
	theme(panel.background = element_rect(fill = "transparent"),
		  legend.title = element_blank(),
		  panel.border = element_rect(colour = "grey", fill = "transparent"),
		  panel.grid = element_blank(),
		  panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
		  axis.text = element_text(colour = "black", size = 7),
		  legend.position = c(-0.01, 0.1),
		  legend.key.size = unit(0.5, "cm"),
		  legend.text = element_text(size = 7),
		  legend.background = element_rect(fill = "transparent", colour = "transparent"),
		  legend.justification = 0,
		  legend.direction = "horizontal",
		  panel.spacing = unit(0.80, "cm")) +
	ggsave(here("Figures", "06_gca-predictions.png"), height = 2.5)
					
#### coefficients ########################################
coefs %>%
	filter(term %in% c("trial_type1", "trial_type2", "lp2-1", "vocab_comp", "trial_type1:lp2-1", "trial_type2:lp2-1")) %>%
	mutate(term = factor(term,
						 levels = c("trial_type1", "trial_type2", "lp2-1", "trial_type1:lp2-1", "trial_type2:lp2-1", "vocab_comp"),
						 labels = c("Prime (NC vs. C)", "Prime (NC/C vs. U)", "Profile (ML vs. BL)", "Prime (NC vs. C) \U000D7 Profile", "Prime (C/NC vs. U) \U000D7 Profile", "Comprehensive vocabulary"))) %>%
	ggplot(aes(estimate, term, colour = term, fill = term)) +
	geom_vline(xintercept = 0, linetype = "dashed") +
	geom_errorbarh(aes(xmin = x2_5_percent, xmax = x97_5_percent), height = 0, size = 5, alpha = 0.5) +
	geom_point(size = 2, colour = "black") +
	geom_errorbarh(aes(xmin = estimate-std_error, xmax = estimate+std_error), colour = "black", height = 0) +
	geom_label(aes(x = -1.15, y = term, label = paste0("p ", printp(pr_z, add_equals = TRUE))), size = 2, colour = "black", fill = "white") +
	labs(x = "Estimated coefficient", y = "Parameter",
		 caption = "Points, whiskers, and boxes indicate point estimates,\n +1/-1 SE, and 95% CIs, respectively.",
		 colour = "Parameter", fill = "Parameter") +
	scale_colour_brewer(palette = "Set1") +
	scale_fill_brewer(palette = "Set1") +
	scale_x_continuous(limits = c(-1.25, 1.75), breaks = seq(-1.5, 1.5, by = 0.5)) +
	theme(panel.background = element_rect(fill = "transparent"),
		  panel.border = element_rect(colour = "grey", fill = "transparent"),
		  panel.grid = element_blank(),
		  panel.grid.major.x = element_line(colour = "grey", size = 0.25, linetype = "dotted"),
		  axis.text = element_text(colour = "black"),
		  legend.position = "none",
		  legend.justification = 0,
		  axis.title = element_blank(),
		  legend.box.background = element_rect(fill = "transparent"),
		  legend.direction = "horizontal",
		  panel.spacing = unit(0.80, "cm")) +
	ggsave(here("Figures", "06_gca-coefs.png"), height = 2.5)

	