# analyse_pupil_adults: Analyse pupil size
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)      # for using pipes
library(tibble)        # for tidy data presentation
library(dplyr)         # for manipulating data
library(tidyr)         # for rehsaping datasets
library(readxl)        # for importing Excel files
library(forcats)       # for working with factors
library(purrr)         # for working with lists
library(ggplot2)       # for visualising data
library(ggthemes)      # for colors in plots
library(PupillometryR) # for analysing pupil data
library(lubridate)     # for working with dates
library(lmerTest)
library(car)

# set paths
file.paths <- list.files('Pupil pilot/Data/data_gaze', full.names = TRUE)

# set experimental parameters
time_bin_duration <- 25 # how long should time bins be in ms?
sampling_rate     <- 120
screenX           <- 1920
screenY           <- 1080
max.frames        <- 4000*(1/sampling_rate)

# create functions
"%!in%"   <- function(x, y) {!(x %in% y)}
source('Code/check_aoi.R')
source('Code/get_dilation_speed.R')
source('Code/filter_dilation_speed.R')

#### import data #######################################

# import trial data
trials <- as.list(c('Pupil pilot/list1.xlsx',      # import stimuli lists
                    'Pupil pilot/list2.xlsx')) %>%
  map(~read_xlsx(.)) %>%
  set_names(c('list1', 'list2')) %>%
  bind_rows(., .id = 'list') %>%
  mutate(trialID = as.character(trialID)) %>%
  select(-trialType)

invalid_participants <- c('cognatepriming1', 'cognatepriming2')

# import and prepare eye-tracker data
data.raw <- map(.x = file.paths,
            ~read.table(.x, sep = ',', dec = '.',
                        header = TRUE, stringsAsFactors = FALSE)) %>%
  bind_rows() %>%
  as_tibble() %>%
  rename(time  = SystemTimeStamp,
         ID    = participant,
         Trial = trialID) %>%
  mutate(
    trialID     = as.character(Trial),
    ID          = paste0("cognatepriming", ID), 
    list        = ifelse(list==1, 'list1', 'list2'),
    meanX       = meanX*screenX,  # change relative coords to screen coords
    meanY       = meanY*screenY,  # change relative coords to screen coords
    lPupil      = ifelse(lPupilV == 0, NA, lPupil), # if non-valid, make NA
    rPupil      = ifelse(rPupilV == 0, NA, rPupil), # if non-valid, make NA
    dateTest    = as.Date(dateTest),
    dateBirth   = as.Date(dateBirth),
    age         = dateTest-dateBirth 
  ) %>%
  group_by(ID, Trial) %>% 
  mutate(
    time = as.numeric(
      cut_interval(
        0:(length(time)-1),
        length = sampling_rate/1000, # transform time to miliseconds 
        labels = FALSE)
      )
  ) %>%
  ungroup() %>%
  select(ID, Trial, phase, time, lPupil, rPupil, trialType,
         meanX, meanY, list, stimulus, meanValidity)

#### filter data ################################################################
data <- data.raw %>%
  filter(
    ID %!in% invalid_participants,
    case_when(phase == "baseline" ~ time <= 1000,
              phase == "target"   ~ time <= 3000,
              TRUE                ~ NA)
    ) %>%
  make_pupillometryr_data(
    data      = .,
    subject   = ID,
    trial     = Trial,
    time      = time,
    condition = trialType
) %>%
  calculate_mean_pupil_size(data = ., 
                            pupil1 = rPupil, 
                            pupil2 = lPupil) %>%
  downsample_time_data(data = .,
                       pupil = mean_pupil,
                       timebin_size = time_bin_duration,
                       option = 'median') %>%
  clean_missing_data(.,
                     pupil = mean_pupil,
                     trial_threshold = .25,
                     subject_trial_threshold = .75) %>%
  filter_data(data = .,
              pupil = mean_pupil,
              filter = 'median',
              degree = 11) %>%
  interpolate_data(data  = .,
                   pupil = mean_pupil,
                   type  = 'linear') %>%
  baseline_data(data  = .,
                pupil = mean_pupil,
                start = 1,
                stop  = 1000)

#### t-tests ######################################################################ç
data_t <- data %>% 
  create_difference_data(data = .,
                         pupil = mean_pupil) %>%
  create_functional_data(
    data = .,
    pupil = mean_pupil,
    basis = 10,
    order = 4
  ) %>%
  run_functional_t_test(
    data = .,
    pupil = mean_pupil,
    alpha = 0.025
  ) 

#### fit model ####################################################################
data_gca <-  data %>%
  mutate(
    trialType = factor(trialType, levels = c("control", "object")),
    ot1       = poly(.$time, 2)[, 1],
    ot2       = poly(.$time, 2)[, 2]
  ) 

model <- lmer(
  mean_pupil ~
    (ot1+ot2)*trialType +
    (ot1+ot2+trialType|ID),
  data = data_gca, REML = FALSE, control = lmerControl(optimizer = "bobyqa")
)

anova <- anova(model, type = 3, ddf = "Satterthwaite")

#### visualise data ###############################################################

# t-tests
plot(data_t, show_divergence = TRUE, colour = '#1B9E77', fill = "#D95F02") +
  geom_hline(yintercept = 0) +
  labs(x = "Time (ms)", y = "Student's t") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 12),
    axis.text = element_text(colour = "black")
  )
  

# participant level
ggplot(data, aes(x = time, y = mean_pupil, colour = trialType, fill = trialType)) +
  facet_wrap(~ID) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(aes(group = Trial), alpha = 0.1) +
  stat_summary(aes(y = fitted(model)),
               fun.data = mean_se, geom = "ribbon", size = 1, colour = NA, alpha = 0.3) +
  stat_summary(aes(y = fitted(model)), fun.y = mean, geom = "line", size = 1) +
  labs(x = "Time (ms)", y = "Baseline corrected mean pupil size (%)",
       colour = "Trial type", fill = "Trial type") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    panel.grid = element_line(colour = "grey", linetype = "dotted"),
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 12),
    axis.text = element_text(colour = "black"),
    legend.position = "top"
  ) +
  ggsave("Figures/pupil_participant.png")

# group level
ggplot(data, aes(x = time, y = mean_pupil, colour = trialType, fill = trialType)) +
  geom_hline(yintercept = 0, colour = "black") +
  stat_summary(aes(y = fitted(model), group = ID),
               fun.y = mean, geom = "line", alpha = 0.2) +
  stat_summary(aes(y = fitted(model)),
               fun.data = mean_se, geom = "ribbon", size = 1, colour = NA, alpha = 0.3) +
  stat_summary(aes(y = fitted(model)), fun.y = mean, geom = "line", size = 1) +
  labs(x = "Time (ms)", y = "Baseline corrected mean pupil size (%)",
       colour = "Trial type", fill = "Trial type") +
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    panel.grid = element_line(colour = "grey", linetype = "dotted"),
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 12),
    axis.text = element_text(colour = "black"),
    legend.position = "top"
  )


ggplot(data) +
  stat_summary(aes(x = time, y = fitted(model), colour = trialType), fun.data = mean_se, geom = "line", size = 1) +
  labs(x = "Time (ms)", y = "Baseline corrected mean pupil size (mm)")


  




plot(t.test, show_divergence = T, colour = 'red', fill = 'orange')







# filter data
data <- data %>%
  group_by(participant, trial) %>%
  mutate(row = 1:length(participant)) %>%
  mutate(pre = case_when(
    row == 1 ~ 0,
    row == max(row) ~ 0,
    TRUE ~ abs((lPupil-lag(lPupil))/(time-lag(time)))
  ),
  post = case_when(row == 1 ~ 0, row == max(row) ~ 0,
    TRUE ~ abs((lPupil-lead(lPupil))/(time-lead(time)))
  )) %>%
  transform(., dilation_speed = pmin(pre, post)) %>%
  mutate(median_abs_deviation = median(abs(dilation_speed - median(data$dilation_speed))))
  filter(
    participant %!in% c('001', '002'), # remove invalid participant (check logs)
    meanValidity,            # gaze is valid
    lPupilValidity == TRUE,
    lPupil>=1.5 & lPupil<=9, # pupil size should be within 1.5 and 9
  )
 



#### fit model ########################################

m0 <- lme4::lmer(meanPupil ~ (ot1+ot2+ot3)*trialType + ((ot1+ot2+ot3)*trialType | trialID:participant),
                 control = lmerControl(optimizer = 'bobyqa', ), data = data.agg, REML = FALSE)

#### visualise data ###################################

# raw data
ggplot(data, aes(time, lPupil, group = trialType, fill = trialType, colour = trialType)) +
  facet_wrap(~participant) +
  stat_summary(fun.data = mean_se, geom = 'ribbon', colour = NA, alpha = 0.7) +
  stat_summary(fun.y = mean, geom = 'line', size = 1) +
  labs(x = 'Time (ms)', y = 'Pupil size', fill = 'Condition', colour = 'Condition') +
  scale_colour_few() +
  scale_fill_few() +
  scale_x_continuous(breaks = seq(0, 50, 4000), limits = c(0, 4000)) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(colour = 'black'),
    legend.justification = c(0, 0),
    legend.position = c(0, 0)
  ) +
  ggsave('figures/pupil_size-raw.png')

# aggregated data
ggplot(data.agg, aes(x = timeBins, y = meanPupil, group = trialType, fill = trialType, colour = trialType)) +
  facet_wrap(~participant) +
  geom_line(aes(group = trialID), colour = 'gray', alpha = 0.3, size = 0.5) +
  stat_summary(fun.data = mean_se, geom = 'ribbon', colour = NA, alpha = 0.7) +
  stat_summary(fun.y = mean, geom = 'line', size = 1) +
  geom_text(x = 22, y = 3.6, label = 'Target onset', angle = 90, size = 3, colour = 'black') +
  geom_vline(xintercept = 20, linetype = 'dotted') +
  labs(x = 'Time bins (50 ms)', y = 'Pupil size', fill = 'Condition', colour = 'Condition') +
  scale_colour_few() +
  scale_fill_few() +
  scale_x_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(colour = 'black'),
    legend.justification = c(0, 0),
    legend.position = c(0, 0)
  ) +
  ggsave('figures/pupil_size.png')

# model
ggplot(data.agg, aes(x = timeBins, y = meanPupil, group = trialType, fill = trialType, colour = trialType)) +
  facet_grid(participant~trialType) +
  geom_line(aes(group = trialID), colour = 'gray', size = 0.5) +
  stat_summary(aes(y = fitted(m0)), fun.y = mean, geom = 'line', size = 1) +
  stat_summary(fun.data = mean_se, geom = 'pointrange', alpha = 0.5) +
  labs(x = 'Time bins (25 ms)', y = 'Pupil size',
       title = 'Pupil size across trials and conditions (n = 1)',
       fill = 'Condition (Mean + SEM)', colour = 'Condition (Mean + SEM)') +
  scale_colour_few() +
  scale_fill_few() +
  scale_x_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(colour = 'black'),
    legend.justification = c(0, 0),
    legend.position = c(0, 0)
  ) +
  ggsave('figures/pupil_size-fitted2.png', width = 10)
