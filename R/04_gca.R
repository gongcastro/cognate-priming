# Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Pompeu Fabra University

#### set up ################################################

# load packages
library(tibble)     # for more informative data sets
library(dplyr)      # for manipulating data
library(tidyr)      # for reshaping datasets
library(data.table) # for importing data
library(ggplot2)    # for visualising data
library(lme4)       # for fitting mixed effect models
library(car)        # for KR-Anova
library(optimx)     # for changing optimiser
library(here)       # for locating files

#### import data ###########################################
data <- fread(here("Data", "04_fixations.txt"), sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
  as_tibble()

# 3rd-order polynomials of the time domain 
t <- poly(unique(data$TimeBin), 4)
data[,paste("ot", 1:4, sep="")] <- t[data$TimeBin, 1:4]

#### fit model #############################################
model <- glmer(
	ProbFix ~
		(ot1+ot2+ot3) + TrialType*LangProfile  +
		(ot1+ot2+ot3 | ParticipantID) +
		(ot1+ot2+ot3 | TrialID) +
		(TrialType | ParticipantID) +
		(LangProfile | ParticipantID) +
		(TrialType | TrialID) +
		(LangProfile | TrialID),
	control   = glmerControl(optimizer="bobyqa"),
	na.action = "na.exclude",
	data      = data,
	family    = binomial(link = "logit")
)

summary(model)

#### visualise data ########################################
ggplot(data, aes(x = TimeBin, y = ProbFix, colour = AOI, fill = AOI)) +
  facet_wrap(~trialType) +
  stat_summary(fun.y = "mean", geom = "point", alpha = 0.5) +
  stat_summary(aes(y = fitted(model), group = AOI),
               fun.y = "mean", geom = "line") +
  labs(x = "Time bin (100 ms)", y = "Probability of target fixation\n(empirical logit)",
       colour = "Trial type", fill = "Trial type") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_line(colour = "grey", linetype = "dotted"),
    text = element_text(size = 12),
    axis.text.x = element_text(colour = "black"),
    legend.position = c(0.6, 0.1),
    legend.direction = "horizontal"
  ) +
  ggsave(here("Figures", "04_gca-predictions.png"))
  


#### explore data ##########################################

# plot pairs
ggpairs(data = CognatePriming, columns = c("Elog", "trialType"))



