# 00_import: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(here)         # for locating files
library(osfr)         # for connecting to Open Science Framework
library(magrittr)     # for using pipes
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(readxl)       # for importing Excel files
library(lubridate)    # for working with dates
library(stringr)      # for working with character strings
library(purrr)        # for working with lists
library(ggplot2)      # for visualising data
library(googledrive)  # for downloading participant-level information
library(eyetrackingR) # for processing eye-tracking data

# load functions
source("Code/osf_download_component.R")
source('Code/target_coords.R')
source('Code/distractor_coords.R')
source('Code/prime_coords.R')
"%!in%" <- function(x, y) !(x %in% y)

# retrieve data from OSF (takes around a minute, you can download it manually)
osf_auth("")
osf_download_component(project = "wekda", component = "Data")

# set experimental parameters
time_bin_duration <- 50  # how long should time bins be in ms?
sampling_rate     <- 120
screenX           <- 1920
screenY           <- 1080

#### import data #######################################

# import participant-level data
participants <- read_xlsx(here::here("Data/Participant data", "data_participants.xlsx")) %>%
  mutate(list = paste0("bcn_", language, list))

# import trial-level data
trials <- as.list(c('Stimuli/list_bcn_cat1.xlsx',
                    'Stimuli/list_bcn_cat2.xlsx',
                    'Stimuli/list_bcn_cat3.xlsx',
                    'Stimuli/list_bcn_spa1.xlsx',
                    'Stimuli/list_bcn_spa2.xlsx',
                    'Stimuli/list_bcn_spa3.xlsx')) %>%
  map(~read_xlsx(.)) %>%
  set_names(c('bcn_cat1', 'bcn_cat2', "bcn_cat3", "bcn_spa1", "bcn_spa2", "bcn_spa3")) %>%
  bind_rows(., .id = 'list') %>%
  mutate(
    trialID   = as.character(trialID),
    height_px = 500,
    width_px  = 500,
    height_mm = 800,
    width_mm  = 800
  )

# import eye-tracking data
data.raw <- map(.x = list.files('Data/Gaze data/Barcelona', full.names = TRUE, recursive = TRUE),
                ~read.table(.x, sep = ',', dec = '.',
                            header = TRUE, stringsAsFactors = FALSE)) %>%
  map(~mutate(., trialType = case_when(trialType == 1 ~ "cognate",
                                       trialType == 2 ~ "noncognate",
                                       trialType == 3 ~ "unrelated",
                                       TRUE           ~ as.character(trialType)))) %>%
  bind_rows() %>%
  as_tibble() %>%
  rename(time  = SystemTimeStamp,
         ID    = participant,
         Trial = trialID
         ) %>%
  mutate(
    Trial       = as.character(Trial),
    ID          = paste0("cognatepriming", ID), 
    meanX       = meanX*screenX,  # change relative coords to screen coords
    meanY       = meanY*screenY,  # change relative coords to screen coords
    lPupil      = ifelse(lPupilV == 0, NA, lPupil), # if non-valid, make NA
    rPupil      = ifelse(rPupilV == 0, NA, rPupil), # if non-valid, make NA
    dateTest    = as.Date(dateTest),
    dateBirth   = as.Date(dateBirth),
    age         = dateTest-dateBirth,
    list        = paste0("bcn_", language, list) 
  ) %>%
  filter(phase %in% c("prime", "target_distractor")) %>%
  group_by(ID, Trial, phase) %>% 
  mutate(
    time = as.numeric(
      cut_interval(
        0:(length(time)-1),
        length = sampling_rate/1000, # transform time to miliseconds 
        labels = FALSE)
    )
  ) %>%
  ungroup()

#### process data ###########################################################
data.processed <- data.raw %>%
  mutate(
    Trial = as.character(Trial),
    # evaluate if gaze is in prime AOI
    gazeP = prime_coords(data = .,
                         x_gaze = meanX,
                         y_gaze = meanY),
    # evaluate if gaze is in target AOI
    gazeT = target_coords(data = .,
                          x_gaze = meanX,
                          y_gaze = meanY,
                          target_location = locationTarget),
    # evaluate if gaze is in distractor AOI
    gazeD = distractor_coords(data = .,
                          x_gaze = meanX,
                          y_gaze = meanY,
                          target_location = locationTarget),
  ) %>%
  select(ID, Trial, phase, time, lPupil, rPupil,
         meanX, meanY, meanValidity,
         list, locationTarget, stimulus,
         gazeP, gazeT, gazeD) %>%
  left_join(., participants, by = c("ID", "list")) %>% # add participant-level information
  left_join(., trials, by = c("list", "Trial" = "trialID")) %>% # add trial-level information
  mutate(meanValidity = meanValidity < 1, # TRUE is non valid
         distance1 = 650, # distance of right eye from screen (for extracting fixations)
         distance2 = 650) # distance of left eye from screen (for extracting fixations)

#### export data ###############################################
write.table(data.raw, file = here::here("Data", "00_raw.txt"), sep = "\t", row.names = FALSE)
write.table(data.processed, file = here::here("Data", "01_processed.txt"), sep = "\t", row.names = FALSE)

