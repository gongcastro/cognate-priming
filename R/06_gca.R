# 04_gca: Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Pompeu Fabra University

#### set up ################################################

# load packages
library(tibble)      # for more informative data sets
library(dplyr)       # for manipulating data
library(tidyr)       # for reshaping datasets
library(data.table)  # for importing data
library(ggplot2)     # for visualising data
library(ggridges)    # for density plots
library(stringr)     # for manipulating character strings
library(lme4)        # for fitting frequentist mixed effect models
library(ggeffects)   # for predicting means
library(car)         # for Wald test
library(arm)         # for model simulations
library(purrr)       # for extracting elements from lists
library(forcats)     # for dealing with categorical variables
library(papaja)      # for printing formatted numbers
library(patchwork)   # for arranging plots
library(RePsychLing) # for Principal Component Analysis
library(here)        # for locating files

#### import data ###########################################
data <- fread(here("Data", "04_prepared.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
	as_tibble() %>%
	distinct(ParticipantID, TargetID, TimeBin, .keep_all = TRUE)

#### fit model #############################################

# maximal model with crossed-random effeto for participant and trial
model <- lmer(
	ElogitFix ~
		(TimeBin1+TimeBin2+TimeBin3)*TrialType*LangProfile  +
		FreqTarget +
		(TimeBin1+TimeBin2+TimeBin3 | ParticipantID) +
		(TimeBin1+TimeBin2+TimeBin3 | ParticipantID:TrialType) +
		(TimeBin1+TimeBin2+TimeBin3 | TargetID) +
		(TimeBin1+TimeBin2+TimeBin3 | TargetID:TrialType),
	weights = 1/Weights,
	REML = FALSE,
	na.action = na.exclude,
	control = lmerControl(
		optimizer = "bobyqa",
		optCtrl = list(maxfun = 20000)
	), 
	data = data
)
summary <- summary(model)               # extract summary of model
isSingular(model)                       # check if Cholesky factor is singular
chf        <- getME(model,"Tlist")[[2]] # Cholesky factor
rowlengths <- sqrt(rowSums(chf*chf))    # unconditional correlation matrix
svd        <- svd(chf, nv = 0)$u        # singular value decomposition - Near-singular matrix
pca        <- rePCA(model)

# extract variance-covariance matrix
vcov <- summary$vcov %>%
	as.matrix() %>%
	as.data.frame() %>%
	rownames_to_column("Variable1") %>%
	pivot_longer(cols = 2:17, names_to = "Variable2", values_to = "Value") %>%
	mutate_at(1:2, ~str_replace_all(., ":", " \U000D7 ")) %>%
	mutate_at(1:2, ~str_remove_all(., "\\(|\\)"))

# extract correlation matrix
correlation <-as.list(summary$varcor) %>%
	map(~attributes(.)) %>%
	map("correlation") %>%
	map(as.data.frame) %>%
	map(~rownames_to_column(., "Term")) %>%
	bind_rows(.id = "FixedEffect") %>%
	mutate_at(1, ~str_replace_all(., ":", " \U000D7 ")) %>%
	rename(Term1 = Term) %>%
	pivot_longer(3:6, names_to = "Term2", values_to = "Correlation") %>%
	mutate_at(1:3, ~str_remove_all(., "\\(|\\)"))

#### coefficients ##########################################
coefs <- summary(model) %$% 
	coefficients %>%
	as.data.frame() %>%
	rownames_to_column("Term") %>%
	rename(SEM = `Std. Error`, t = `t value`) %>%
	mutate(Term = str_remove_all(Term, "\\(|\\)"))

# following Gelman & Hill (2007)
sims <- sim(model, 1000) # simulate
# estimation uncertainty of the fixed-effect coefficients
sims.fixef <- sims@fixef %>%
	as.data.frame() %>%
	pivot_longer(everything(), names_to = "Term", values_to = "Value") %>%
	mutate(Term = str_remove_all(Term, "\\(|\\)")) %>%
	mutate_at(1, ~str_replace_all(., ":", " \U000D7 ")) 
	

sims.sigma <- sims@sigma # estimation uncertainty of the in the residual stantard deviation parameter

#### confidence intervals ##################################
confints <- confint.merMod( # calculate confidence intervals
	model,  
	method = "Wald",
	level = 0.95
) %>%
	as.data.frame() %>%
	rownames_to_column(var = "Term") %>%
	slice(42:57) %>%
	rename(ci1 = `2.5 %`, ci2 = `97.5 %`) %>%
	mutate(Term = str_remove_all(Term, "\\(|\\)"))

#### anova #################################################
anova <- Anova(model, type = "III", test.statistic = "Chisq") %>%
	rename(p = `Pr(>Chisq)`) %>%
	as.data.frame() %>%
	rownames_to_column("Term")

#### check for multicollinearity ###########################
multicollinearity <- vif(model) %>%
	as.data.frame() %>%
	rownames_to_column("Term") %>%
	rename(VIF = ".") %>%
	add_row(Term = "Intercept", VIF = NA_real_, .before = 1)

#### check normality of residuals ##########################
residuals <- rstudent(model)

#### merge results #########################################
results <- anova %>% # perform type III ANOVA (KF F-test)
	right_join(., coefs, by = "Term") %>% # join outcome with coefficients
	left_join(., confints, by = "Term") %>% # join outcome with confidence intervals
	left_join(., multicollinearity, by = "Term") %>% # join outcome with multicollinearity checks
	dplyr::select(Term, Chisq, Df, Estimate, SEM, p, ci1, ci2, VIF) %>%
	mutate_at(1, ~str_replace_all(., ":", " \U000D7 ")) %>%
	mutate(Term = str_remove_all(Term, "\\(|\\)"))

#### extract effects #######################################
effects <- data %>%
	mutate(
		Fitted = fitted(model),
		Residuals = residuals,
		LangProfile = ifelse(LangProfile==0, "Monolingual", "Bilingual"),
		TrialType = case_when(
			TrialType==0 ~ "Unrelated",
			TrialType==1 ~ "Non-cognate",
			TrialType==2 ~ "Cognate",
			TRUE         ~ NA_character_
		),
		Language = ifelse(Language==-0.5, "Spanish", "Catalan"),
		FittedOdds = exp(Fitted),
		FittedProb = FittedOdds/(1+FittedOdds)
	)

#### export results ########################################	
fwrite(results, here("Data", "05_gca-results.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(effects, here("Data", "05_gca-predictions.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(vcov, here("Data", "05_gca-vcov.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(correlation, here("Data", "05_gca-correlation.txt"), sep = "\t", dec = ".", row.names = FALSE)
fwrite(multicollinearity, here("Data", "05_gca-multicollinearity.txt"), sep = "\t", dec = ".", row.names = FALSE)

#### visualise results #####################################
	
# variance-covariance matrix	
vcov %>%
	ggplot(aes(x = fct_inorder(Variable1), y = fct_rev(fct_inorder(Variable2)), fill = Value)) +
	geom_raster() +
	labs(fill = "Variance/covariance") +
	scale_fill_viridis_c() +
	theme(
		text = element_text(size = 20),
		axis.text = element_text(colour = "black", size = 12),
		axis.title = element_blank(),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "white"),
		axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
		legend.position = "top"
	) +
	ggsave(here("Figures", "05_gca-vcov.png"), height = 5, width = 8)

# correlation matrix
correlation %>%
	ggplot(aes(x = fct_inorder(Term1), y = fct_rev(fct_inorder(Term2)), fill = Correlation)) +
	facet_wrap(~FixedEffect) +
	geom_raster() +
	geom_text(aes(label = paste0("r = ", printnum(Correlation, digits = 2))), colour = "white") +
	labs(fill = "Correlation") +
	scale_fill_viridis_c() +
	theme(
		text = element_text(size = 20),
		axis.text = element_text(colour = "black", size = 12),
		axis.title = element_blank(),
		panel.grid = element_blank(),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "white"),
		axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)
	) +
	ggsave(here("Figures", "05_gca-correlation.png"))

# residuals
effects %>%
	ggplot(aes(x = Residuals)) +
	geom_density(alpha = 0.5, fill = "black") +
	stat_function(fun = dnorm, n = 100, args = list(mean = 0, sd = 1), linetype = "dashed") +
	labs(x = "Studentised residual", y = "Probability density") +
	theme(
		text = element_text(size = 20),
		panel.grid = element_line(colour = "grey", linetype = "dotted"),
		axis.text = element_text(colour = "black", size = 12),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey")
	) +
	effects %>%
	ggplot(aes(sample = Residuals)) +
	geom_qq(colour = "black", alpha = 0.5, size = 0.5) +
	geom_qq_line() +
	labs(x = "Theoretical normal distribution", y = "Observed normal distribution") +
	plot_layout() +
	theme(
		text = element_text(size = 15),
		panel.grid = element_line(colour = "grey", linetype = "dotted"),
		axis.text = element_text(colour = "black", size = 12),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey")
	) +
	ggsave(here("Figures", "05_gca-residuals.png"))

# predictions by Participant
ggplot(effects, aes(x = Time, y = ProbFix,
					group = ParticipantID,
					colour = TrialType,
					fill = TrialType)) +
	facet_grid(~LangProfile) +
	stat_summary(aes(group = ParticipantID), fun.y = "mean", geom = "line", alpha = 0.50) +
	stat_summary(aes(group = TrialType), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
	stat_summary(aes(group = TrialType), fun.y = "mean", geom = "line", size = 1) +
	labs(x = "Time (ms)", y = "Probability of target fixation",
		 fill = "Trial type", colour = "Trial type") +
	geom_hline(yintercept = 0.50) +
	scale_colour_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	theme(
		text = element_text(size = 20),
		axis.text = element_text(colour = "black"),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "right"
	) +
	ggsave(here("Figures", "05_gca-predictions-participant.png"))

ggplot(data, aes(x = TimeBin, y = ElogitFix, colour = TrialType, fill = TrialType)) +
	facet_wrap(~LangProfile) +
	stat_summary(fun.data = "mean_se", geom = "pointrange")  +
	stat_summary(aes(y = fitted(model)), fun.y = "mean", geom = "line", alpha = 0.5) 
	
# predictions by Trial
ggplot(effects, aes(x = Time, y = FittedProb,
					group = TargetID,
					colour = TrialType,
					fill = TrialType)) +
	facet_grid(~LangProfile) +
	stat_summary(aes(group = TargetID), fun.y = "mean", geom = "line", alpha = 0.25) +
	stat_summary(aes(group = TrialType), fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5) +
	stat_summary(aes(group = TrialType), fun.y = "mean", geom = "line", size = 1) +
	labs(x = "Time (ms)", y = "Probability of target fixation",
		 fill = "Trial type", colour = "Trial type") +
	geom_hline(yintercept = 0.50) +
	scale_colour_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	theme(
		text = element_text(size = 20),
		axis.text = element_text(colour = "black"),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "right"
	) +
	ggsave(here("Figures", "05_gca-predictions-trial.png"))
	
# coefficients (dist)
sims.fixef %>%
	left_join(results, by = "Term") %>%
	ggplot(aes(x = Value, y = fct_rev(fct_inorder(Term, ordered = TRUE)), fill = factor(stat(quantile)))) +
	stat_density_ridges(geom = "density_ridges_gradient", rel_min_height = 0.01, panel_scaling = 1.5,
						quantile_lines = TRUE, quantiles = c(0.025, 0.975),
						calc_ecdf = TRUE, vline_size = 0) +
	geom_vline(xintercept = 0, linetype = "dashed") +
	scale_y_discrete(expand = expand_scale(mult = c(0, 0.15))) +
	scale_fill_manual(values = c("black", "grey", "black")) +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black", size = 12),
		axis.title.y = element_blank(),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank(),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "none"
	) +
	ggsave(here("Figures", "05_gca-coefficients-dist.png"))

# coefficients (freq)
results %>%
	mutate(Term = factor(Term, ordered = TRUE)) %>%
	ggplot(aes(x = fct_rev(fct_inorder(Term, ordered = TRUE)), y = Estimate)) +
	geom_errorbar(aes(ymin = ci1, ymax = ci2, width = 0, size = 5), alpha = 0.5) +
	geom_errorbar(aes(ymin = Estimate-SEM, ymax = Estimate+SEM), width = 0, size = 1) +
	geom_point(size = 2) +
	geom_hline(yintercept = 0, linetype = "dashed") +
	labs(x = "Fixed effect", y = "Estimate") +
	coord_flip() +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black", size = 12),
		axis.title.y = element_blank(),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.grid.major.y = element_blank(),
		panel.grid.minor.y = element_blank(),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "none"
	) +
	ggsave(here("Figures", "05_gca-coefficients.png"))

# coefficient uncertainty
sims.fixef %>%
	filter(Term %in% c("TrialType", "LangProfile", "TrialType × LangProfile")) %>%
	mutate(Simulation = rep(1:1000, 3)) %>%
	pivot_wider(id_cols = "Simulation", names_from = "Term", values_from = "Value") %>%
	ggplot(aes(x = TrialType, y = LangProfile, fill = stat(level))) +
	stat_density_2d(geom = "polygon") +
	labs(x = "Trial type", y = "Language Profile", fill = "Counts") +
	scale_fill_viridis_c() +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black"),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "right",
		legend.direction = "horizontal",
		legend.key.size = unit(1, "cm"),
	) +
	sims.fixef %>%
	filter(Term %in% c("TrialType", "LangProfile", "TrialType × LangProfile")) %>%
	mutate(Simulation = rep(1:1000, 3)) %>%
	pivot_wider(id_cols = "Simulation", names_from = "Term", values_from = "Value") %>%
	ggplot(aes(x = TrialType, y = `TrialType × LangProfile`, fill = stat(level))) +
	stat_density_2d(geom = "polygon") +
	labs(x = "Trial type", y = "TrialType × LangProfile", fill = "Counts") +
	scale_fill_viridis_c() +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black"),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "none"
	) +
	sims.fixef %>%
	filter(Term %in% c("TrialType", "LangProfile", "TrialType × LangProfile")) %>%
	mutate(Simulation = rep(1:1000, 3)) %>%
	pivot_wider(id_cols = "Simulation", names_from = "Term", values_from = "Value") %>%
	ggplot(aes(x = LangProfile, y = `TrialType × LangProfile`, fill = stat(level))) +
	stat_density_2d(geom = "polygon") +
	labs(x = "Language profile", y = "TrialType × LangProfile", fill = "Counts") +
	scale_fill_viridis_c() +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black"),
		panel.grid = element_line(linetype = "dotted", colour = "grey"),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey"),
		legend.position = "none"
	) +
	guide_area() +
	plot_layout(guides = "collect") +
	plot_annotation(tag_levels = "A") +
	ggsave(here("Figures", "05_gca-coefficients-uncertainty.png"))

	