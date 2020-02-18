# stimuli_duration: Calculate duration of audio files
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(audio)      # for acoustic analysis
library(data.table) # for importing data
library(tibble)     # for tidy data presentation
library(magrittr)   # for using pipes ('%>%')
library(readxl)     # for importing Excel files
library(dplyr)      # for manipulating data
library(tidyr)      # for manipulating datasets
library(purrr)      # for working with lists
library(forcats)    # for working with categorical variables
library(stringr)    # for working with character strings
library(here)       # for locating files
library(ggplot2)    # for visualising data
library(ggalt)      # for plotting lollipops
library(patchwork)  # for arranging plots

# load/create functions
source(here("R", "Functions", "rebuild_time.R"))
get_rate <- attr_getter("rate")
"%!in%" <- function(x, y) !(x %in% y)


#### get stimuli lists #######################################
# make long version of stimuli lists
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>%
	filter(ValidTrial) %>%
	select(TrialID, Location, Language, Version, List, TrialType, Audio, Target)

#### calculate durations #####################################
durations <- tibble(
	Filename = list.files(here("Stimuli", "Sounds"), recursive = TRUE)  %>% str_extract("/.*") %>% str_remove("/") , # locate .wav files
	Path = list.files(here("Stimuli", "Sounds"), recursive = TRUE, full.names = TRUE), # locate .wav files
	Word = Filename %>% str_remove(".wav") # word id
) %>%
	mutate(
		Location = ifelse(str_detect(Path, "barcelona"), "Barcelona", "Oxford"),
		Language = case_when(
			str_detect(Path, "cat") ~ "Catalan",
			str_detect(Path, "spa") ~ "Spanish",
			TRUE                    ~ "English"
		),
		Duration = unlist(
			map2(.x = map(map(file.paths, load.wave) %>% set_names(file.names), length), # get time domain of each audio from sample rate and number of measurements
				 .y = map(map(file.paths, load.wave) %>% set_names(file.names), get_rate),
				 .f = function(x = .x, y = .y) x*(1/y)*0.5
			)  
		)
	) %>%
	select(Word, Filename, Duration, Location, Language, Path)

#### merge data #############################################
data <- left_join(durations, trials, by = c("Location", "Language", "Word" = "Target", "Filename" = "Audio")) %>%
	drop_na(Location) %>%
	mutate(
		Duration = Duration, # in stereo audios, both tracks are concatenated by default, so we need to split the value in half
		Duration = ifelse(Location=="Oxford", Duration-5, Duration),
		Overlap = Duration-0.7,
		IsOverlapping = Overlap>0
	) 

#### summarise stats #########################################
summary <- data %>%
  group_by(Language, Location, Version, TrialType) %>%
  summarise(
    m_duration        = mean(Duration, na.rm = TRUE),
    sd_duration       = sd(Duration, na.rm = TRUE),
    min_duration      = min(Duration, na.rm = TRUE),
    min_duration_word = Word[which.min(Duration)],
    max_duration      = max(Duration, na.rm = TRUE),
    max_duration_word = Word[which.max(Duration)],
    mean_overlap      = mean(Overlap, na.rm = TRUE),
    min_overlap       = mean(Overlap, na.rm = TRUE),
    min_overlap_word  = Word[which.max(Overlap)],
    max_overlap       = mean(Overlap, na.rm = TRUE),
    max_overlap_word  = Word[which.max(Overlap)]
  )

#### visualise data ##########################################

# durations
durations <- data %>%
	drop_na() %>%
	filter(Version %!in% c("i", "latin")) %>%
	ggplot(aes(x = TrialType, y = Duration, colour = TrialType, fill = TrialType)) +
	facet_grid(Location~Language, drop = TRUE) +
	geom_violin(colour = NA) +
	geom_boxplot(fill = "white", colour = "black", width = 0.1, outlier.size = 0.5) +
	geom_point(alpha = 0.5, size = 0.5, colour = "grey") +
	scale_colour_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	labs(x = "Trial type", y = "Duration (s)\n",
		 colour = "Trial type", fill = "Trial type") +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black"),
		axis.text.x = element_blank(),
		legend.position = "top",
		legend.direction = "vertical",
		panel.grid = element_line(colour = "grey", linetype = "dotted"),
		panel.grid.major.x = element_blank(),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey")
	)

# overlap distribution
overlap_dist <- data %>%
	drop_na() %>%
	filter(Version %!in% c("i", "latin")) %>%
	ggplot(aes(x = Overlap, colour = TrialType, fill = TrialType)) +
	facet_grid(Location~Language) +
	geom_density(alpha = 0.5) +
	labs(x = "Overlap with Target-Distractor (s)", y = "Density\n") +
	scale_colour_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black"),
		legend.position = "none",
		panel.grid = element_line(colour = "grey", linetype = "dotted"),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey")
	)
# overlap
overlap <-data %>%
	drop_na() %>%
	filter(Version %!in% c("i", "latin")) %>%
	ggplot(aes(x = Word, y = Duration, colour = TrialType, fill = TrialType)) +
	facet_grid(TrialType~Language) +
	geom_lollipop(point.size = 0.5) +
	geom_hline(yintercept = 0.7, linetype = "dashed") +
	labs(x = "Word", y = "Overlap with Target-Distractor (s)",
		 colour = "Trial type", fill = "Trial type") +
	scale_colour_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	coord_flip() +
	theme(
		text = element_text(size = 15),
		axis.text = element_text(colour = "black"),
		axis.text.y = element_blank(),
		axis.title.y = element_blank(),
		legend.position = "none",
		panel.grid = element_blank(),
		axis.ticks.y = element_blank(),
		panel.background = element_rect(fill = "transparent"),
		panel.border = element_rect(fill = "transparent", colour = "grey")
	) 

durations + overlap_dist + overlap + guide_area() +
	plot_layout(guides = "collect", widths = c(2, 2, 3, 1), ncol = 2, nrow = 2) +
	plot_annotation(tag_levels = "A") +
	ggsave(here("Figures", "Stimuli", "duration-overlap.png"), height = 8) 

	
	
#### export data #############################################
fwrite(data, here("Data", "Stimuli", "stimuli_duration-raw.txt"), sep = '\t', row.names = FALSE)
fwrite(summary, here("Data", "Stimuli", "stimuli_duration-aggregated.txt"), sep = '\t', row.names = FALSE)
