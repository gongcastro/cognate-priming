# 02_gaze: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)     # for using pipes
library(here)         # for locating files
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(purrr)        # for working with lists
library(eyetrackingR) # for processing eye-tracking data
library(ggplot2)      # for data visualisation
library(patchwork)    # for arranging plots
library(stringr)      # for working with character strings
library(forcats)      # for dealing with NAs
library(osfr)         # for connecting to Open Science Framework
library(readxl)       # for importing Excel files
library(lubridate)    # for working with dates
library(googledrive)  # for downloading participant-level information

# load functions
source("Code/osf_download_component.R")
source('Code/target_coords.R')
source('Code/distractor_coords.R')
source('Code/prime_coords.R')
"%!in%" <- function(x, y) !(x %in% y)

# set parameters
time_bin_duration <- 50
screenX           <- 1920
screenY           <- 1080
sampling_rate     <- 120

#### import data ################################################3
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
  mutate(phase = ifelse(phase == "BLANK", "blank", phase)) %>%
  group_by(ID, Trial, phase) %>% 
  mutate(
    time = as.numeric(
      cut_interval(
        0:(length(time)-1),
        length = sampling_rate/1000, # transform time to miliseconds 
        labels = FALSE)
    )
  ) %>%
  filter(phase %in% c("prime", "blank", "BLANK", "audio"),
         (phase=="prime" & between(time , 0, 1500)) | 
           (phase=="blank" & between(time , 0, 50))  |
           (phase=="audio" & between(time , 0, 700))
  ) %>%
  ungroup()

#### process data ###########################################################
data.processed <- data.raw %>%
  mutate(
    Trial = as.character(Trial),
    # evaluate if gaze is in prime AOI
    gazeP = prime_coords(data = ., x_gaze = meanX, y_gaze = meanY)
  ) %>%
  select(ID, Trial, phase, time, lPupil, rPupil,
         meanX, meanY, meanValidity,
         list, locationTarget, stimulus,
         gazeP) %>%
  left_join(., participants, by = c("ID", "list")) %>% # add participant-level information
  left_join(., trials, by = c("list", "Trial" = "trialID")) %>% # add trial-level information
  mutate(meanValidity = meanValidity < 1, # TRUE is non valid
         distance1 = 650, # distance of right eye from screen (for extracting fixations)
         distance2 = 650, # distance of left eye from screen (for extracting fixations)
         phase = factor(phase, levels = c("prime", "blank", "audio"), ordered = TRUE)
  )
#### filter data #########################################################

# list of trials with <75% looking time to prime in prime phase
exclude_prime <- data.processed %>%
  group_by(ID, Trial) %>%
  filter(phase == "prime", gazeP == 1) %>%
  summarise(n = n()) %>%
  mutate(valid = (n >= ((1500/1000)*(sampling_rate)*0.75))) %>%
  filter(valid == FALSE) %>%
  ungroup()

# list of participants with <50% valid trials regaring prime looking time
exclude_prime_ID <- exclude_prime %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  mutate(include = (n < (32*0.50))) %>%
  filter(include == FALSE) %>%
  ungroup()

#### filter data  ############################################

data.filtered <- data.processed %>%
  # remove participants labelled as non-valid
  filter(valid) %>%
  # remove trials with <75% looking time to prime in prime phase
  anti_join(., exclude_prime, by = c("ID", "Trial")) %>%
  # remove participants with <50% valid trials regaring prime looking time
  anti_join(., exclude_prime_ID, by = "ID") %>%
  # remove trials where participants were unfamiliar with any of the words
  filter(
    !str_detect(prime, unfamiliarCat),
    !str_detect(prime, unfamiliarSpa),
    !str_detect(target, unfamiliarCat),
    !str_detect(target, unfamiliarSpa),
    !str_detect(distractor, unfamiliarCat),
    !str_detect(distractor, unfamiliarSpa)
  ) %>%
  # remove trial with <75% valid samples during target and distractor
  # remove participants with <50% valid trials during
  filter(phase %in% c("prime", "blank", "audio"), time >= 0, time <= 3050) 

#### analyse trackloss ###################################################
trackloss <- trackloss_analysis(data.filtered)

#### process #############################################################
data <- data.filtered %>%
  mutate(trialType = factor(trialType, levels = c("unrelated", "noncognate", "cognate")),
         profile = case_when(spanish > 0.80 ~ "monolingual",
                             catalan > 0.80 ~ "monolingual",
                             TRUE           ~ "bilingual"))

#### reconstruct gaze ####################################################
data %>%
  mutate(
    OnsetSection = case_when(between(time, 0, 100)   ~ "0-100 ms",
                                  between(time, 100, 200) ~ "100-200 ms",
                                  between(time, 200, 300) ~ "200-300 ms",
                                  between(time, 300, 400) ~ "300-400 ms",
                                  between(time, 400, 500) ~ "400-500 ms",
                                  TRUE                    ~ ">500 ms") %>%
      factor(., levels = c("0-100 ms","100-200 ms","200-300 ms",
                           "300-400 ms","400-500 ms",">500 ms"), ordered = TRUE)
         ) %>% 
  ggplot(aes(x = meanX, y = meanY)) +
  facet_grid(phase~OnsetSection) +
  geom_rect(aes(xmin = 0, xmax = screenX, ymin = 0, ymax = screenY),
            fill = "#7F7F7F", colour = "black") +
  geom_bin2d() +
  geom_rect(aes(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
            fill = "transparent", colour = "white") +
  labs(x = "Gaze position in X axis (px)", y = "Gaze coordinates in Y axis (px)",
       fill = "Count") +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(limits = c(0, screenX), breaks = c(0, screenX)) +
  scale_y_continuous(limits = c(0, screenY), breaks = c(0, screenY)) +
  coord_fixed() +
  theme(
    panel.background = element_rect(fill = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggsave(here::here("Figures", "03_gaze-screen-prime.png"))
