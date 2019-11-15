# stimuli_stats: Retrieve stimuli stats by list
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(magrittr) # for working with pipes
library(dplyr)    # for manipulating data
library(tidyr)    # for manipulating datasets
library(reshape2) # for manipulating datasets
library(readxl)   # for importing Excel spreadsheets
library(tibble)   # for tidy data presentation
library(forcats)  # for working with factors
library(purrr)    # for working with lists
library(ggplot2)  # for visualising data

#### import pool #########################################
pool <-
  read.delim('~/projects/devlex/data/pool.txt', fileEncoding = 'macintosh', stringsAsFactors = FALSE) %>% 
  select(-contains('english'), -contains('aoa')) %>%
  as_tibble() %>%
  gather('language', 'word',
         -c(freq_spanish, freq_catalan,
            gender_spanish, gender_catalan,
            syllables_spanish, syllables_catalan,
            phonemes_spanish, phonemes_catalan,
            understands18spanish, understands24spanish, understands28spanish,
            understands18catalan, understands24catalan, understands28catalan,
            levenshtein)) %>%
  select(word, 1:ncol(.), -language) 

#### import stimuli lists #################################

# make long version of stimuli lists
trials.long <-
  # import stimuli lists
  as.list(c('/Volumes/GONZALOGC/biprime/stimuli/stimuli_stats_catalan.xlsx',
            '/Volumes/GONZALOGC/biprime/stimuli/stimuli_stats_spanish.xlsx')) %>%
  map(~read_xlsx(.)) %>%
  map(~mutate(., trialID = as.factor(trialID))) %>%
  set_names(c('catalan', 'spanish')) %>%
  bind_rows(., .id = 'language') %T>%
  assign('trials.wide', ., envir = .GlobalEnv) %>% # save intermediate wide version
  gather(role, 'word', -c(language, trialID, trialType)) %>%
  select(word, language, trialID, role, trialType)

#### join stimuli and stats ################################

# wide version
stats.wide <- trials.wide %>%
  bind_rows(.id = 'language') %>%
  # add prime stats
  left_join(., pool, by = c('prime' = 'word')) %>%
  distinct(prime, distractor, .keep_all = TRUE) %>%
  select(trialID, prime, target, distractor, -trialType, 1:ncol(.)) %>%
  rename_at(9:ncol(.), list(~paste0('prime_', .))) %>%
  # add target stats
  left_join(., pool, by = c('target' = 'word')) %>%
  distinct(target, distractor, .keep_all = TRUE) %>%
  select(trialID, prime, target, distractor, 1:ncol(.)) %>%
  rename_at(22:ncol(.), list(~paste0('target_', .))) %>%
  # add distractor stats
  left_join(., pool, by = c('distractor' = 'word')) %>%
  distinct(target, distractor, .keep_all = TRUE) %>%
  select(trialID, prime, target, distractor, 1:ncol(.)) %>%
  rename_at(35:ncol(.), list(~paste0('distractor_', .)))

# long version
stats.long <- trials.long %>%
  left_join(., pool, by = 'word') %>%
  distinct(word, language, trialID, role, .keep_all = TRUE) %>%
  mutate(
    gender       = ifelse(language=='catalan', gender_catalan, gender_spanish),
    syllables    = ifelse(language=='catalan', syllables_catalan, syllables_spanish),
    phonemes     = ifelse(language=='catalan', phonemes_catalan, phonemes_spanish),
    frequency    = ifelse(language=='catalan', freq_catalan, freq_spanish),
    understand18 = ifelse(language=='catalan', understands18catalan, understands18spanish),
    understand24 = ifelse(language=='catalan', understands24catalan, understands24spanish),
    understand28 = ifelse(language=='catalan', understands28catalan, understands28spanish)
  ) %>%
  select(word, language, trialID, trialType, role,
         gender, syllables, phonemes, frequency,
         understand18, understand24, understand28,
         levenshtein)

#### summarise stimuli stats ###############################
stats.summary.overall <- stats.long %>%
  filter(role %in% c('prime', 'target', 'distractor')) %>%
  group_by(language, trialType, role) %>%
  summarise(
    mean_syllables    = mean(syllables, na.rm = TRUE),
    min_syllables     = min(syllables, na.rm = TRUE),
    max_syllables     = max(syllables, na.rm = TRUE),
    mean_phonemes     = mean(phonemes, na.rm = TRUE),
    min_phonemes      = min(syllables, na.rm = TRUE),
    max_phonemes      = max(syllables, na.rm = TRUE),
    mean_frequency    = mean(frequency, na.rm = TRUE),
    min_frequency     = min(frequency, na.rm = TRUE),
    max_frequency     = max(frequency, na.rm = TRUE),
    mean_understand24 = mean(understand24, na.rm = TRUE),
    min_understand24  = min(understand24, na.rm = TRUE),
    max_understand24  = max(understand24, na.rm = TRUE),
    mean_understand28 = mean(understand28, na.rm = TRUE),
    min_understand28  = min(understand28, na.rm = TRUE),
    max_understand28  = max(understand28, na.rm = TRUE)
  ) %T>%
  assign('stats.summary.png', .) %>%
  ungroup() %>%
  group_by(language, trialType) %>%
  summarise(
    mean_syllables    = mean(mean_syllables, na.rm = TRUE),
    min_syllables     = min(min_syllables, na.rm = TRUE),
    max_syllables     = max(max_syllables, na.rm = TRUE),
    mean_phonemes     = mean(mean_phonemes, na.rm = TRUE),
    min_phonemes      = min(min_phonemes, na.rm = TRUE),
    max_phonemes      = max(max_phonemes, na.rm = TRUE),
    mean_frequency    = mean(mean_frequency, na.rm = TRUE),
    min_frequency     = min(min_frequency, na.rm = TRUE),
    max_frequency     = max(max_frequency, na.rm = TRUE),
    mean_understand24 = mean(mean_understand24, na.rm = TRUE),
    min_understand24  = min(min_understand24, na.rm = TRUE),
    max_understand24  = max(max_understand24, na.rm = TRUE),
    mean_understand28 = mean(mean_understand28, na.rm = TRUE),
    min_understand28  = min(min_understand28, na.rm = TRUE),
    max_understand28  = max(max_understand28, na.rm = TRUE)
  ) 

#### export data ###########################################
write.table(stats.wide, 'data/stimuli_stats-wide.txt', sep = '\t', dec = '.', row.names = FALSE, fileEncoding = 'macintosh')
write.table(stats.long, 'data/stimuli_stats-long.txt', sep = '\t', dec = '.',  row.names = FALSE, fileEncoding = 'macintosh')
write.table(stats.summary, 'data/stimuli_stats-summary.txt', sep = '\t', dec = '.',  row.names = FALSE, fileEncoding = 'macintosh')
write.table(stats.summary.overall, 'data/stimuli_stats-summary-ov.txt', sep = '\t', dec = '.',  row.names = FALSE, fileEncoding = 'macintosh')

#### visualise data ########################################

# frequency
stats.long %>%
  filter(role %in% c('prime', 'target', 'distractor')) %>%
  ggplot(., aes(trialType, frequency)) +
  facet_grid(language~factor(role, levels = c('prime', 'target', 'distractor'))) +
  geom_point(alpha = 0.5, size = 2.5, show.legend = FALSE) +
  stat_summary(aes(group = trialType), fun.data = mean_se, geom = 'crossbar', colour = 'red', width = 0.5) +
  labs(x = 'Trial type',
       y = 'Frequency (Zipf score from SUBTLEX)',
       title = 'Word frequency by role, trial type, and language',
       subtitle = 'Red boxes represent the mean and SEM') +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = 'black'),
        legend.position = 'top',
        panel.grid.major.x = element_blank()) +
  ggsave('figures/stimuli_stats_frequency.png')

# (Orthographic) Levenshtein's distance
stats.long %>%
  filter(role %in% c('prime', 'target', 'distractor')) %>%
  ggplot(., aes(trialType, levenshtein)) +
  facet_grid(language~factor(role, levels = c('prime', 'target', 'distractor'))) +
  geom_point(alpha = 0.5, size = 2.5, show.legend = FALSE) +
  stat_summary(aes(group = trialType), fun.data = mean_se, geom = 'crossbar', colour = 'red', width = 0.5) +
  labs(x = 'Trial type',
       y = " Orthographical Levenshtein's distance",
       title = 'Translation equivalent linguistic distance by role, trial type, and language',
       subtitle = 'Red boxes represent the mean and SEM') +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = 'black'),
        legend.position = 'top',
        panel.grid.major.x = element_blank()) +
  ggsave('figures/stimuli_stats_levenshtein.png')

# Familiarity at 24 months of age
stats.long %>%
  filter(role %in% c('prime', 'target', 'distractor')) %>%
  ggplot(., aes(trialType, understand24)) +
  facet_grid(language~factor(role, levels = c('prime', 'target', 'distractor'))) +
  geom_point(alpha = 0.5, size = 2.5, show.legend = FALSE) +
  stat_summary(aes(group = trialType), fun.data = mean_se, geom = 'crossbar', colour = 'red', width = 0.5) +
  labs(x = 'Trial type',
       y = " % of ~824mo toddlers reported to understand the word",
       title = 'Familiarity with the word at 24 months of age',
       subtitle = 'Red boxes represent the mean and SEM') +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = 'black'),
        legend.position = 'top',
        panel.grid.major.x = element_blank()) +
  ggsave('figures/stimuli_stats_understand24.png')

# Familiarity at 28 months of age
stats.long %>%
  filter(role %in% c('prime', 'target', 'distractor')) %>%
  ggplot(., aes(trialType, understand24)) +
  facet_grid(language~factor(role, levels = c('prime', 'target', 'distractor'))) +
  geom_point(alpha = 0.5, size = 2.5, show.legend = FALSE) +
  stat_summary(aes(group = trialType), fun.data = mean_se, geom = 'crossbar', colour = 'red', width = 0.5) +
  labs(x = 'Trial type',
       y = " % of ~28mo toddlers reported to understand the word",
       title = 'Familiarity with the word at 26.5-31 months of age',
       subtitle = 'Red boxes represent the mean and SEM') +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme(text = element_text(size = 12),
        axis.text = element_text(colour = 'black'),
        legend.position = 'top',
        panel.grid.major.x = element_blank()) +
  ggsave('figures/stimuli_stats_understand28.png')
