# 02_fixations: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(here)         # for locating files
library(magrittr)     # for using pipes
library(readxl)       # for importing Excel files
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(purrr)        # for working with lists
library(eyetrackingR) # for processing eye-tracking data
library(gazepath)     # for identiying fixations
library(ggplot2)      # for data visualisation

# set parameters
time_bin_duration <- 100  # how long should time bins be in ms?
sampling_rate     <- 120  # how many samples are takien per second?
screenX           <- 1920 # screen width in pixels
screenY           <- 1080 # screen heigh in pixels
reolution         <- 23   # screen size in inches

# load functions
source('Code/target_coords.R')     # for evaluating whether gaze is in target
source('Code/distractor_coords.R') # for evaluating whether gaze is in distractor

#### import data ###########################################################
data <- read.table(here("Data", "02_filtered.txt"), sep = "\t", header = TRUE) %>%
  make_eyetrackingr_data(data               = .,
                         participant_column = "ID",
                         trackloss_column   = "meanValidity",
                         time_column        = "time",
                         trial_column       = "Trial",
                         aoi_columns        = c("gazeT", "gazeD"),
                         treat_non_aoi_looks_as_missing = FALSE) %>%
  # keep only time window of interest and re-center the time
  subset_by_window(rezero = TRUE,
                   remove = TRUE,
                   window_start_time = 0,
                   window_end_time = 2000) %>%
  mutate(
    Trial     = as.character(Trial),
    ID        = as.character(ID),
    TrialType = factor(trialType, levels = c("unrelated", "noncognate", "cognate")), # define language profile
    Profile   = case_when(spanish > 0.80 ~ "Monolingual",
                          catalan > 0.80 ~ "Monolingual",
                          TRUE           ~ "Bilingual"),
    Language = ifelse(language=="cat", "Catalan", "Spanish") # relabel languages
  ) %>%
  rename(trial = Trial) %>%
  select(ID, trial, time, meanX, meanY, distance1, locationTarget, Language, Profile) %>%
  as.data.frame()

# import trial data
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
    height_mm = 500*(25.4/dpi),
    width_mm  = 500*(25.4/dpi)
  ) %>%
  as.data.frame()

#### identify fixations ####################################################
fixation.info <- split(data, f = data$ID) %>% # separate each participant's dataset
  map(., ~gazepath( # get fixations in each dataset
  data       = .,
  x1         = "meanX",         # gaze horizontal position
  y1         = "meanY",         # gaze vertical position 
  d1         = "distance1",     # variable containing distance from screen
  trial      = "trial",         # trial index in dataset
  height_mm  = 132.2917,        # AOI height in mm
  width_mm   = 132.2917,        # AOI width in mm
  height_px  = 500,             # AOI height in px 
  width_px   = 500,             # AOI width in px
  samplerate = 120,             # eye-tracker sampling rate
  method     = "gazepath",      # select algorithm
  posthoc    = TRUE,            # merge consecutive fixations?
  thres_dur  = 100,             # minimum fixation duration
  in_thres   = 200,
  extra_var  = c("ID", "trial") # keep these variables
)) %>%
  map("fixations")              # extract fixation info
  map(~bind_rows(.)) %>%        # merge fixations within participant
  bind_rows(.) %>%              # merge fixations across participants
  filter(Value == "f") %>%      # exclude saccades
  select(                       # select variables of interest
    ID, Trial = trial,
    Start, End) %>%
  as_tibble() %>%               # for more informative datasets
  mutate_if(.predicate = is.factor, as.character) %>%
  group_by(ID, Trial) %>% # whatever we do next, do it spearately for each combination of these variables
  mutate(FixNum = row_number()) %>% # index all fixations within each trial
  ungroup()

# reconstruct fixations across time domain
fixations <- fixation.info %>%
  right_join(., data, by = c("ID", "Trial" = "trial")) %>%   # merge fixation info with time data
  left_join(., trials, by = c("Trial" = "trialID")) %>%      # merge trial-level data
  mutate(Fixation = (time >= Start) & (time <= End)) %>%     # is time point within fixation boundaries?
  group_by(ID, Trial, time, trialType,
           list, locationTarget, Language, Profile) %>%      # prepare to agreggate by time point
  summarise(Fixation = any(Fixation),                        # is this time point within any of the fixation boundaries?
            meanX    = first(meanX),                         # preserve gaze location in X-axis
            meanY    = first(meanY)) %>%                     # preserve gaze location in Y-axis
  ungroup() %>%
  mutate_if(.predicate = is.factor, as.character) %>%
  mutate(
    # evaluate if gaze is in target of distractor using a custom script (in pilot, target location was generated online)
    FixTarget     = target_coords(data = ., x_gaze = meanX, y_gaze = meanY, target_location = locationTarget),
    FixDistractor = distractor_coords(data = ., x_gaze = meanX, y_gaze = meanY, target_location = locationTarget),
    # generate time bins
    TimeBin       = as.numeric(cut(time, breaks = seq(0, 2000, by = 100), labels = seq(1, 2000/100)))
  ) %>%
  # aggregate across trials
  group_by(ID, trialType, TimeBin, Language, Profile) %>%
  summarise(
    TotalSamples      = n(),               # number of samples in this time bin for this participant
    SamplesTarget     = sum(FixTarget),    # number of samples in ta
    SamplesDistractor = sum(FixDistractor)
  ) %>%
  mutate(
    PFixTarget = SamplesTarget/TotalSamples,
    PFixDistractor = SamplesDistractor/TotalSamples
  ) %>%
  pivot_longer( # make long format
    cols      = c("PFixTarget", "PFixDistractor"),
    names_to  = "AOI",
    values_to = "ProbFix"
  ) %>%
  mutate(AOI = ifelse(AOI == "PFixTarget", "Target", "Distractor"),
         Time = TimeBin*time_bin_duration)
  
#### visualise data ##########################################

# Raw empirical logit gainst time
ggplot(fixations, aes(x = Time, y = ProbFix, colour = AOI, fill = AOI)) +
  facet_wrap(~trialType) +
  stat_summary(fun.data = "mean_se", geom = "ribbon", colour = NA, alpha = 0.5, na.rm = TRUE) +
  stat_summary(fun.y = "mean", geom = "line", na.rm = TRUE) +
  labs(x = "Time (ms)", y = "Prob. Fixation",
       colour = "Trial type", fill = "Trial type") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_line(colour = "grey", linetype = "dotted"),
    text = element_text(size = 12),
    axis.text.x = element_text(colour = "black"),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  ggsave(here("Figures", "03_fixations-time.png"))

# reconstruct gaze
fixations <- fixation.info %>%
  right_join(., data, by = c("ID", "Trial" = "trial")) %>% # merge fixation info with time data
  mutate(Fixation = (time >= Start) & (time <= End)) %>%
  group_by(ID, Trial, time, locationTarget, Start, End) %>%
  summarise(Fixation = any(Fixation),
            FixNum = first(FixNum),
            meanX = first(meanX),
            meanY = first(meanY)) %>%
  filter(Fixation) %>%
  mutate(
    Section = case_when(between(End, 0, 100)    ~ "0-100 ms",
                        between(End, 100, 200)  ~ "100-200 ms",
                        between(End, 200, 300)  ~ "200-300 ms",
                        between(End, 300, 400)  ~ "300-400 ms",
                        between(End, 400, 500)  ~ "400-500 ms",
                        between(End, 500, 600)  ~ "500-600 ms",
                        between(End, 600, 700)  ~ "600-700 ms",
                        between(End, 700, 1000) ~ "700-1000 ms",
                        TRUE                    ~ ">1000 ms") %>%
      factor(., levels=c("0-100 ms","100-200 ms",
                         "200-300 ms","300-400 ms",
                         "400-500 ms","500-600 ms",
                         "600-700 ms", "700-1000 ms",
                         ">1000 ms"))
  ) %>%
  ggplot(aes(x = meanX, y = meanY)) +
  facet_wrap(~Section) +
  geom_rect(xmin = 280, xmax = 780, ymin = 290, ymax = 790,
            fill = "transparent", colour = "white") +
  geom_rect(xmin = 1140, xmax = 1640, ymin = 290, ymax = 790,
            fill = "transparent", colour = "white") +
  stat_bin_2d(binwidth = 100, na.rm = TRUE) +
  labs(x = "X coordinates (pixels", y = "Y coordinates", colour = "Fixation") +
  scale_fill_viridis_c(option = "magma") +
  labs(x = "X coordinates (pixels)", y = "Y coordinates (pixels)",
       fill = "Fixation samples") +
  scale_x_continuous(limits = c(0, screenX)) +
  scale_y_continuous(limits = c(0, screenY)) +
  coord_fixed() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#7F7F7F"),
    panel.grid =  element_blank(),
    panel.border = element_rect(colour = "black", fill = "transparent"),
    legend.position = "top",
  ) +
  ggsave(here("Figures/03_fixations-screen.png"))
  
#### export data ##############################################
write.table(fixation.info, here("Data", "03_fixation-info.txt"), sep = "\t", dec = ".", row.names = FALSE)
write.table(fixations, here("Data", "03_fixations.txt"), sep = "\t", dec = ".", row.names = FALSE)



