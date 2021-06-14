# stimuli

# set up ----

# load packages
library(tidyverse)
library(readxl)
library(multilex)
library(here)

# set parameters
individual_plots <- FALSE

# process stimuli
stimuli <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>% 
	mutate(version) %>% 
	left_join(select(multilex::pool, item, language, frequency_zipf),
			  by = c("target_cdi" = "item", "test_language" = "language"))

# export
saveRDS(stimuli, here("Data", "Stimuli", "stimuli.rds"))

