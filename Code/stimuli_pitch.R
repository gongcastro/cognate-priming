# srimuli_pitch: pitch analysis by word (F0)
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ################################################

# load packages
library(PraatR)    # for acoustic analysis
lirary(tibble)     # for tidy datasets
library(dplyr)     # for wrangling data
library(readr)     # for working with strings
library(tidyr)     # for separating columns
library(magrittr)  # for manipulating data with pipes
library(purrr)     # for working with lists
library(readxl)    # for importing Excel data
library(ggplot2)   # for data visualization 
library(ggthemes)  # for colours in plots

#### set paths #################################################
file.names  <- list.files('/Volumes/GONZALOGC/biprime/stimuli/others/recording3/05_intensity', full.names = FALSE) # locate .wav files
file.paths  <- list.files('/Volumes/GONZALOGC/biprime/stimuli/others/recording3/05_intensity', full.names = TRUE)  # build path for each file
words       <- sub('.wav', '', file.names) %>% sub('05_', '', .) %>% sub('.wav', '', .)                                                 # build path for each file
pitch.paths <- paste0('/Volumes/GONZALOGC/biprime/stimuli/others/recording3/06_pitch/Pitch/', words, '.Pitch')                       # build path for outputs
pitch.tier.paths <- paste0('/Volumes/GONZALOGC/biprime/stimuli/others/recording3/06_pitch/PitchTier/', words, '.PitchTier')                       # build path for outputs
n <- length(words)

#### get fundamental frequency/pitch ###########################
pitch.arguments <- list(0.001, # time step in seconds (0.0 = auto)
                        75,    # pitch floor in Hz
                        800)   # pitch ceiling in Hz


for (i in 1:n){
  # create a .PitchTier file for each audio
  praat(
    command = 'To Pitch...',
    input = file.paths[i],
    output = pitch.paths[i],
    arguments = pitch.arguments,
    overwrite = TRUE
  )
  
  # create a .PitchTier file for each audio
  praat(
    command = 'Down to PitchTier',
    input = pitch.paths[i],
    output = pitch.tier.paths[i],
    filetype = 'headerless spreadsheet',
    overwrite = TRUE
  )
  
  # show progress message
  message(paste0(i, '/', n, ' pitches extracted'))
  
}

# import the .PitchTier files containing the F0 data
pitch <-
  map(pitch.tier.paths, ~read.table(., col.names = c('time', 'f0'))) %>% # read the .Pitch object from the pitch.tier.path 
  map(~as_tibble(.)) %>%                                                 # trasnform it to tibble (easier to read)
  set_names(., words) %>%                                                # name each dataset as file
  bind_rows(., .id = 'names') %>%                                        # join data from all files
  rename(word = 'names') %>%
  separate(word, c('word', 'language'), '_') %>%                         # make a distinct variable for word and language
  mutate(time = time*1000)

# create a table with all values
pitch.medians <-
  pitch %>%
  group_by(word, language) %>%
  summarise(f0 = mean(f0, na.rm = TRUE)) %>% # get the median pitch for each file
  mutate(
    grand_median = median(.$f0),             # get the grand mean (median pitch of all files)
    factor     = (grand_median/f0)*100,      # if we multiply the median pitch of this file for this factor, it will yield the grand mean pitch (for Praat)
    target     = f0*(factor/100)             # test that the previous statement is true
)


#### export data ######################################################################
write.table(pitch, '/Volumes/GONZALOGC/biprime/stimuli/others/recording3/06_pitch/pitch.txt', sep = '\t', row.names = FALSE)
write.table(pitch.medians, '/Volumes/GONZALOGC/biprime/stimuli/others/recording3/06_pitch/pitch_medians.txt', sep = '\t', row.names = FALSE)

#### visualise data ###################################################################
ggplot(pitch, aes(x = time, y = f0, colour = language)) +
  geom_line(aes(group = word), size = 0.5, alpha = 0.4) +
  labs(x = 'Time (ms)', y = 'Fundamental Frequency (Hz)', colour = 'Language') +
  scale_colour_few() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(colour = 'black'),
    legend.position = 'top'
  ) +
  facet_wrap(~language, ncol = 1)
  
