#### analysis: Analyse data ####################################################

#### set up ####################################################################

# load packages
library(tidyverse)
library(data.table)
library(lme4)
library(lmerTest)
library(here)

# set params

#### import data ---------------------------------------------------------------
by_trial <- fread(here("Results", "by_trial.csv")) %>% 
	as_tibble() %>% 
	rowwise() %>% 
	mutate(proportion = target/n) %>% 
	mutate_at(vars(lp, trial_type), as.factor) %>% 
	filter((target+distractor) >= 0.75*n) %>% 
	ungroup()
contrasts(by_trial$lp) <- c(-0.5, 0.5)
contrasts(by_trial$trial_type) <- cbind(c(-0.5, -0.5, 1), c(0.5, -0.5, 0))

by_time_bin <- fread(here("Results", "by_time_bin.csv")) %>% 
	as_tibble() %>% 
	mutate(time_bin1 = poly(time_bin, 3, simple = TRUE)[,1],
		   time_bin2 = poly(time_bin, 3, simple = TRUE)[,2],
		   time_bin3 = poly(time_bin, 3, simple = TRUE)[,3]) %>%
	mutate_at(vars(lp, trial_type), as.factor)

contrasts(by_time_bin$lp) <- c(-0.5, 0.5)
contrasts(by_time_bin$trial_type) <- cbind(c(-0.5, -0.5, 1), c(0.5, -0.5, 0))

#### trial-wise ----------------------------------------------------------------
trial0 <- glmer(cbind(target, n) ~ 1 + (1 | participant),
				family = binomial("logit"),
				control = glmerControl(optimizer = "bobyqa"),
				data = by_trial)
trial1 <- glmer(cbind(target, n) ~ 1 + age_group + (1 | participant),
				family = binomial("logit"),
				control = glmerControl(optimizer = "bobyqa"),
				data = by_trial)
trial1 <- glmer(cbind(target, n) ~ age_group*lp + (1 | participant),
				family = binomial("logit"),
				control = glmerControl(optimizer = "bobyqa"),
				data = by_trial)
trial2 <- glmer(cbind(target, n) ~ age_group*lp*trial_type + (1+trial_type| participant),
				family = binomial("logit"),
				control = glmerControl(optimizer = "bobyqa"),
				data = by_trial)

anova <- anova(trial0, trial1, trial2)

ggplot(by_trial, aes(trial_type, p_distractor)) +
	facet_wrap(~lp) +
	stat_summary(aes(y = predict(trial2), group = trial_type), fun = mean, geom = "line") +
	geom_point(shape = 1, stroke = 1, alpha = 0.5)


#### time bin-wise -------------------------------------------------------------
time0 <- glmer(cbind(target, n) ~
			   	(time_bin1+time_bin2+time_bin3)*trial_type +
			   	(1 + time_bin1 | participant),
			   data = by_time_bin,
			   family = binomial("logit"))

nd <- expand.grid(
	time_bin1 = seq(min(by_time_bin$time_bin1, na.rm = TRUE),
					max(by_time_bin$time_bin1, na.rm = TRUE),
					by = 0.001),
	time_bin2 = seq(min(by_time_bin$time_bin2, na.rm = TRUE),
					max(by_time_bin$time_bin2, na.rm = TRUE),
					by = 0.001),
	time_bin3 = seq(min(by_time_bin$time_bin3, na.rm = TRUE),
					max(by_time_bin$time_bin3, na.rm = TRUE),
					by = 0.001),
	participant = unique(by_time_bin$participant),
	trial_type = unique(by_time_bin$trial_type)
) %>% 
	mutate(p = predict(time0, newdata = .))

ggplot(by_time_bin, aes(time_bin1, target/n, colour = trial_type)) +
	stat_summary(fun.data = "mean_se", geom = "pointrange") +
	#stat_summary(aes(y = fitted(time0, newdata = nd)), fun = "mean", geom = "line") +
	labs(x = "Time bin (100 ms)", y = "P(Target)", colour = "Trial type") +
	theme_minimal() +
	theme(axis.title = element_text(face = "bold"),
		  legend.title = element_blank(),
		  strip.background = element_rect(fill = "grey", colour = NA),
		  legend.position = "none")

