# analysis

# set up ----

# load packages
library(tidyverse)
library(eyetrackingR) # for processing eye-tracking data
library(lme4) # for fitting multilevel models
library(lmerTest) # for performing F-tests on lmer coefficientes
library(patchwork) # for arranging plots together
library(janitor) # for cleaning colnames
library(here) # for locating files

# functions
logit_to_prob <- function(x) exp(x) / (1 + exp(x))

# import data ----
participants <- readRDS(here("Data", "Participants", "participants.rds"))
processed <- list(
	Barcelona = readRDS(here("Data", "Gaze", "processed_barcelona.rds")),
	Oxford = readRDS(here("Data", "Gaze", "processed_oxford.rds"))
) %>% 
	bind_rows(.id = "location") %>% 
	# remove CDI labels from item names
	mutate_at(vars(prime, target), function(x) str_remove_all(x, "spa_|cat_")) %>% 
	mutate(
		# needed for eyetrackingR - TRUE means trackloss, FALSE means valid gaze
		trackloss_col = ifelse(valid_sample == TRUE, FALSE, TRUE)
	) %>% 
	# to avoid issues with column names
	rename(time_stamp = time) %>% 
	# eyetrackingR only accepts unique IDs, so from now on we treat longitudinal participants as distinct
	group_by(participant) %>% 
	mutate(participant_unique = case_when(
		date_test==unique(date_test)[2] ~ paste0(participant, "_2"),
		date_test==unique(date_test)[3] ~ paste0(participant, "_3"),
		TRUE ~ paste0(participant, "_1")
	)) %>% 
	ungroup()

# attrition
attrition <- processed %>% 
	mutate(valid_other = !(valid_gaze & !valid_trial)) %>% 
	distinct(participant, age_group, lp, trial_type, trial_num, valid_gaze, valid_vocab, valid_other, valid_trial, valid_participant)
saveRDS(attrition, here("Results", "attrition.rds"))

# to eyetrackingR format
d <- processed %>% 
	filter(valid_participant, valid_trial) %>% 
	make_eyetrackingr_data(
		participant_column = "participant_unique",
		trial_column = "trial_num",
		time_column = "time_stamp",
		trackloss_column = "trackloss_col",
		item_columns = c("prime", "target"),
		aoi_columns = c('aoi_target','aoi_distractor'), 
		treat_non_aoi_looks_as_missing = FALSE
	) %>% 
	subset_by_window(rezero = TRUE, window_start_time = 0.25, window_end_time = 1.8) %>% 
	make_time_sequence_data(
		time_bin_size = 0.1, 
		predictor_columns = c("test_language", "location", "trial_type", "age_group", "lp"),
		aois = "aoi_target"
	) %>% 
	as_tibble() %>% 
	# add previous ID to link longitudinal participants
	left_join(distinct(processed, participant, participant_unique, vocab_size)) %>% 
	clean_names() %>% 
	select(participant, location, age_group, lp, vocab_size, prime, target, trial_num, trial_type,
		   time_bin, ot1, ot2, ot3, prop, weights, elog, logit_adjusted) %>%
	mutate_at(vars(age_group, lp, prime, target, trial_num, trial_type), as.factor)

# set a prior contrasts and orthogonal polynomials
contrasts(d$lp) <- c(-0.5, 0.5)
contrasts(d$trial_type) <- cbind(c(-0.5, -0.5, 1), c(0.5, -0.5, 0))
contrasts(d$age_group) <- contr.poly(3, contrasts = TRUE)
d$vocab_size <- d$vocab_size-mean(d$vocab_size, na.rm = TRUE)

write.csv(d, here("Data", "Gaze", "processed.csv"), row.names = FALSE)
saveRDS(d, here("Data", "Gaze", "processed.rds"))

# fit model ----
fit <- lmer(
	elog ~ age_group + vocab_size + trial_type*lp*(ot1+ot2+ot3) +
		(1+ot1+ot2+ot3+trial_type+age_group | participant) +
		(1+ot1+ot2+ot3+trial_type | target),
	control = lmerControl(optimizer = "bobyqa"),
	data = d
)
saveRDS(fit, here("Results", "fit.rds"))

# coefficients ----
s <- summary(fit)

# confindence intervals
confints <- confint(fit, method = "Wald") %>% 
	as.data.frame() %>% 
	rownames_to_column("term") %>% 
	filter(!str_detect(term, "sig")) %>% 
	clean_names()
colnames(confints) <- c("term", "conf_lower", "conf_upper")

# fixed effects
fixed_effects <- s$coefficients %>% 
	as.data.frame() %>% 
	clean_names() %>% 
	rownames_to_column("term") %>% 
	left_join(confints)

# random effects
random_effects <- lapply(
	ranef(fit),
	function(x) as.data.frame(x) %>% clean_names() %>% rownames_to_column("group")
)

# variance-covariance matrices
cor_matrix <- VarCorr(fit)

# export
saveRDS(fixed_effects, here("Results", "fixed-effects.rds"))
saveRDS(random_effects, here("Results", "random-effects.rds"))
saveRDS(cor_matrix, here("Results", "random-effects-matrix.rds"))

# plot fixed effects
ggplot(fixed_effects, aes(estimate, term, colour = term, fill = term)) +
	geom_vline(xintercept = 0) +
	geom_errorbarh(aes(xmin = estimate-std_error, xmax = estimate+std_error), 
				   height = 0, size = 1) +
	geom_errorbarh(aes(xmin = conf_lower, xmax = conf_upper),
				   height = 0, size = 5, alpha = 0.5) +
	geom_point() +
	labs(x = "Estimate", y = "Term") +
	theme_bw() +
	theme(
		legend.position = "none",
		axis.title.y = element_blank()
	)

# plot random effects
random_effects$target %>% 
	pivot_longer(-group, names_to = "param", values_to = "estimate") %>% 
	ggplot(aes(estimate, group, colour = param)) +
	facet_grid(~param, scales = "free_x") +
	geom_vline(xintercept = 0) +
	geom_point(shape = 1, size = 2, stroke = 1) +
	labs(x = "Estimate", y = "Target picture", colour = "Predictor") +
	
	random_effects$participant %>% 
	pivot_longer(-group, names_to = "param", values_to = "estimate") %>% 
	ggplot(aes(estimate, group, colour = param)) +
	facet_grid(~param, scales = "free") +
	geom_vline(xintercept = 0) +
	geom_point(shape = 1, size = 2, stroke = 1) +
	labs(x = "Estimate", y = "Participant", colour = "Predictor") +
	
	plot_layout(guides = "collect") &
	theme_bw() &
	theme(axis.text.y = element_text(size = 7))


# marginal effects ----
# make predictions and transform them to probability
y_pred <- logit_to_prob(predict(fit))

d %>% 
	drop_na() %>% 
	ggplot(aes(x = ot1, y = prop, colour = trial_type, fill = trial_type)) +
	facet_grid(lp~age_group) +
	stat_summary(fun.data = mean_se, aes(y = y_pred), geom = "ribbon", alpha = 0.5, colour = NA) +
	stat_summary(fun = mean, aes(y = y_pred), geom = "line", size = 1) +
	stat_summary(fun.data = mean_se, geom = "pointrange", size = 0.25) +
	labs(x = "Time bin (100 ms)", y = "Prob. of target fixation",
		 colour = "Trial type", fill = "Trial type") +
	scale_color_brewer(palette = "Set1") +
	scale_fill_brewer(palette = "Set1") +
	scale_y_continuous(limits = c(0, 1)) + 
	scale_x_continuous(breaks = unique(d$ot1), labels = unique(d$time_bin)) +
	theme_bw() +
	theme(
		legend.position = "top",
		panel.background = element_rect(fill = "white", colour = NA),
		strip.background = element_rect(colour = "grey"),
		panel.border = element_rect(colour = "grey"),
		panel.grid = element_blank(),
		panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
		panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
		axis.text.x = element_text(size = 7)
	)



