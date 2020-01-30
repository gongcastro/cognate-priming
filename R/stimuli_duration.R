# stimuli_duration: Calculate duration of audio files
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(audio)     # for acoustic analysis
library(tibble)    # for tidy data presentation
library(magrittr)  # for using pipes ('%>%')
library(dplyr)     # for manipulating data
library(tidyr)     # for manipulating datasets
library(purrr)     # for working with lists
library(readxl)    # for importing Excel spreadsheets
library(forcats)   # for working with categorical variables
library(stringr)   # for working with character strings
library(stringi)   # for working with character strings
library(ggplot2)   # for visualising data
library(ggalt)     # for plotting lollipops
library(patchwork) # for arranging plots

# specify paths
file.names <- list.files('/Volumes/GONZALOGC/BiPrime/stimuli/sounds/04_denoised')                      # locate .wav files
file.paths <- list.files('/Volumes/GONZALOGC/BiPrime/stimuli/sounds/04_denoised', full.names = TRUE)   # build path for each file
words      <- str_replace_all(file.names, '04_|.wav', '') # word id

#### get stimuli lists #######################################

# make long version of stimuli lists
trials.long <-
  # import stimuli lists
  as.list(c('/Volumes/GONZALOGC/biprime/stimuli/stimuli_stats_catalan.xlsx',
            '/Volumes/GONZALOGC/biprime/stimuli/stimuli_stats_spanish.xlsx')) %>%
  map(~read_xlsx(.)) %>%
  map(~mutate(., trialID = as.factor(trialID))) %>%
  set_names(c('catalan', 'spanish')) %T>%
  assign('trials.wide', ., envir = .GlobalEnv) %>% # save intermediate wide version
  bind_rows(., .id = 'language') %>%
  gather(role, 'word', -c(language, trialID, trialType)) %>%
  select(word, language, trialID, role, trialType)

trials.long$word <- stri_trans_general(trials.long$word, "latin-ascii")

#### get durations ###########################################
amplitude   <- map(file.paths, load.wave) %>% set_names(words) # extract amplitude from each audio
n.amplitude <- map(amplitude, length)                          # number of measurements in each audio
sample.rate <- amplitude[[1]]$rate                             # get the sample rate of each audio
time <- map(n.amplitude, .f = function(x) (1:x/sample.rate))   # get time domain of each audio from sample rate and number of measurements

duration <-
  unlist(map(time, ~last(.))) %>%                              # get max time    
  unlist() %>%                           
  tibble(word = names(.), time = .) %>%
  separate(word, c('word', 'language'), '_') %>%
  mutate(
    time = time*1000/2, # in stereo audios, both tracks are concatenated by default, so we need to split the value in half
    overlap = time-700
  ) %>% 
  right_join(., trials.long, by = c('word', 'language')) %>%
  filter(!grepl('-latin|2', word),
         role == 'target')

#### summarise stats #########################################
summary <-
  duration %>%
  group_by(language, trialType) %>%
  summarise(
    mean_duration     = mean(time, na.rm = TRUE),
    sd_duration       = sd(time, na.rm = TRUE),
    min_duration      = min(time, na.rm = TRUE),
    min_duration_word = word[which.min(time)],
    max_duration      = max(time, na.rm = TRUE),
    max_duration_word = word[which.max(time)],
    mean_overlap      = mean(overlap, na.rm = TRUE),
    min_overlap       = mean(overlap, na.rm = TRUE),
    min_overlap_word  = word[which.max(overlap)],
    max_overlap       = mean(overlap, na.rm = TRUE),
    max_overlap_word  = word[which.max(overlap)]
  )

#### visualise data ##########################################
ggplot(duration, aes(trialType, time)) +
  facet_wrap(~language) +
  geom_hline(yintercept = 700, linetype = 'dotted') +
  geom_point(size = 2, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = 'crossbar', colour = 'red', width = 0.5, size = 0.75) +
  geom_text(aes(x = 2, y = 750, label = 'Target image onset (700 ms)')) +
  labs(x = 'Condition', y = 'Duration (ms)',
       title = 'Spoken word duration by condition and language',
       subtitle = 'Red boxes indicate Mean and SEM') +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(colour = 'black')
  ) +
  ggplot(duration, aes(trialType, overlap)) +
  facet_wrap(~language) +
  geom_point(size = 2, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = 'crossbar', colour = 'red', width = 0.5, size = 0.75) +
  labs(x = 'Condition', y = 'Overlap (ms)',
       title = 'Spoken word overlap with taget image onset (at 700 ms) by condition and language',
       subtitle = 'Red boxes indicate Mean and SEM') +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(colour = 'black')
  ) +
  plot_layout(nrow = 2) +
  ggsave('figures/stimuli_sounds_duration.png')
  
#### export data #############################################
write.table(duration, 'data/stimuli_duration.txt', sep = '\t', row.names = FALSE, fileEncoding = 'macintosh')
write.table(summary, 'data/stimuli_duration-summary.txt', sep = '\t', row.names = FALSE, fileEncoding = 'macintosh')
