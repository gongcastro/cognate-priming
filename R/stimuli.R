# stimuli

# set up ----

# load packages
library(tidyverse)
library(readxl)
library(here)

# set parameters
individual_plots <- FALSE

# process stimuli
stimuli <- read_xlsx(here("Stimuli", "stimuli.xlsx"))

# export
saveRDS(stimuli, here("Data", "Stimuli", "stimuli.rds"))

