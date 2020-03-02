# stimuli_stats: Retrieve stimuli stats by list
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(magrittr)   # for working with pipes
library(data.table) # for importing and exporting data
library(dplyr)      # for manipulating data
library(tidyr)      # for manipulating datasets
library(stringdist) # for manipulating datasets
library(readxl)     # for importing Excel spreadsheets
library(tibble)     # for tidy data presentation
library(ggplot2)    # for visualising data
library(here)       # for locating files

#### import data #########################################
# import audio durations
durations <- fread(here("Data", "Stimuli", "stimuli_duration-raw.txt")) %>%
	as_tibble() %>%
	select(-Word)

# import trials
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx"))

# import pool
pool <- fread('~/projects/BiLexicon/Pool/01_pool-stats.txt', stringsAsFactors = FALSE) %>% 
  as_tibble() 

#### merge data #########################################
data <- left_join(trials, pool, by = c("Language", "PrimeCDI" = "Item")) %>%
	# add prime and target frequencies
	rename(FreqPrime = FreqZipf) %>%
	left_join(., dplyr::select(pool, Language, Item, FreqZipf), by = c("Language", "TargetCDI" = "Item")) %>%
	rename(FreqTarget = FreqZipf) %>%
	# add Levenshtein distance for Prime and Prime2
	mutate(LevenshteinPrime = stringdist(Prime, Prime2, method = "lv")) %>%
	# merge for audio durations
	left_join(., durations)
	
#### export data ########################################
fwrite(data, here("Data", "Stimuli", "02_stimuli-stats.txt"))
