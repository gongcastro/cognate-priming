#### stimuli_words: List words included ###############
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu

#### set up ###########################################
# load packages
library(dplyr)      # for manipulating data
library(readxl)     # for importing Excel files
library(tidyr)      # for reshaping datasets
library(data.table) # for importing and exporting data
library(stringr)    # for working with character strings
library(here)       # for locating files

#### import data ######################################
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx"))

#### process data #####################################
words <- trials %>%
	select(PrimeCDI, Prime2CDI, TargetCDI, Target2CDI) %>%
	pivot_longer(cols = everything(), names_to = "Role", values_to = "Item") %>%
	distinct(Item) %>%
	mutate(Language = ifelse(str_detect(Item, "cat_"), "Catalan", "Spanish")) %>%
	select(Language, Item) %>%
	arrange(Language, Item)

#### export data ######################################
fwrite(words, here("Stimuli", "word_list.csv"), sep = ",")

