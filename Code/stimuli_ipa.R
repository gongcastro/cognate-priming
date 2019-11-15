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

  read_xlsx('/Volumes/GONZALOGC/biprime/stimuli/stimuli_main.xlsx', sheet = 'IPA') %>%
  left.join(.,   read_xlsx('/Volumes/GONZALOGC/biprime/stimuli/stimuli_main.xlsx', sheet = 'IPA') %>%
)
  gather(role, 'word', -c(language, trialID, trialType)) %>%
  select(word, language, trialID, role, trialType)