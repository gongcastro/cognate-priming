# Growth Curve Analysis
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Pompeu Fabra University

#### set up ################################################

# load packages
library(here)    # for locating files
library(ggplot2) # for visualising data
library(GGally)  # for plotting model coefficients
library(lme4)    # for fitting mixed effect models

#### import data ###########################################

CognatePriming <- read.table(here("Data", "Fixations"), sep = "\t", stringsAsFactors = FALSE)

#### explore data ##########################################+

# plot pairs
ggpairs(data = CognatePiming, columns = c(TargetFixation, Relatedness, Cognateness))



