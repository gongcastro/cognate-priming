# Power analysis (based on DeBruine & Barr, 2019)
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Pompeu Fabra University

#### set up #############################################

# load packages
library(dplyr)       # for manipulating data
library(magrittr)    # for using pipes
library(tidyr)       # for reshaping datasets
library(lmer)        # for Linear Mixed-Effects Models
library(pwr)         # for power analysis
library(ANOVApower)  # for power analysis
library(broom.mixed) # for tidy lmer output

#### analyse ANOVA power ################################
cells <- expand.grid(
  TrialType = c("unrelated", "non-cognate", "cognate"),
  LP        = c("Spanish-English", "Spanish-Catalan"),
  Age       = c(24, 30)
  ) %>%
  select(Age, LP, TrialType)

labelnames <- cells %>%
  unite("label", c(Age, LP, TrialType), sep = "_") %>%
  pull(label)

comparisons <- ((2*2*2)^2)-((2*2*2)/2)

ANOVApower::ANOVA_design(
  design = "2b*2b*2w",
  n = 20,
  mu = c(65, 60, 55,
         65, 60, 50,
         70, 65, 60,
         70, 65, 55),
  sd = 5,
  r = rep(0.95, comparisons),
  labelnames = labelnames,
  plot = TRUE
)
