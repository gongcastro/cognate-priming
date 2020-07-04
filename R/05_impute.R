# 05_prepares: Impute data
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################
# load packages
library(data.table) # for importing and exporting data
library(dplyr)      # for manipulating data
library(tidyr)      # for reshaping datasets
library(tibble)     # for more informative data frames
library(mice)       # for imputing data
library(ggplot2)    # for visualising data
library(here)       # for locating files

#### import data ########################################
data <- fread(here("Data", "04_prepared.txt")) %>%
	as_tibble()

#### impute data
data.imputed <- mice(
	data = data,
	m = 5,
	method = "pmm",
	maxit = 5,
	seed = 888
	)

data.imputed$imp
