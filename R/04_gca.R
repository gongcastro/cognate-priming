# Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Pompeu Fabra University

#### set up ################################################

# load packages
library(here)    # for locating files
library(tibble)  # for more informative data sets
library(dplyr)   # for manipulating data
library(tidyr)   # for reshaping datasets
library(ggplot2) # for visualising data
library(GGally)  # for plotting model coefficients
library(lme4)    # for fitting mixed effect models
library(lmerTest)
library(optimx)

#### import data ###########################################

data <- read.table(here("Data", "03_fixations.txt"),
                   sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
  as_tibble()

t <- poly(unique(data$TimeBin), 4)
data[,paste("ot", 1:4, sep="")] <- t[data$TimeBin, 1:4]


#### fit model #############################################
model <- lmer(
  formula = ProbFix ~ (ot1+ot2+ot3)*trialType*A + (ot1+ot2+ot3|ID),
  control = lmerControl(optimizer="bobyqa"),
  REML = TRUE,
  na.action = "na.exclude",
  data = data
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



