#### stimuli: analyse audio files ----------------------------------------------

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(ggdist)
library(patchwork)
library(here)

# load/create functions
source(here("R", "utils.R"))

# set parameters
individual_plots <- FALSE

#### import data ---------------------------------------------------------------

# list of words
trials <- read_xlsx(here("Stimuli", "stimuli.xlsx")) %>% 
	filter(location=="BCN") %>% 
	distinct(audio, test_language, trial_type) %>% 
	mutate(file = ifelse(
		test_language=="Catalan",
		paste0("cat_", audio),
		paste0("spa_", audio)
	))

# get audio data
audio_data <- extract_formants(file = here("Stimuli", "Audios")) %>% 
	mutate(file = paste0(file, ".wav")) %>% 
	right_join(trials) 

#### analyse formats -----------------------------------------------------------
amplitude <- audio_data %>% 
	left_join(trials) 

amplitude_means <- amplitude %>%
	filter(intensity > 0) %>%
	group_by(audio, test_language, trial_type) %>%
	summarise(
		duration = max(time, na.rm = TRUE),
		intensity = mean(intensity, na.rm = TRUE),
		.groups= "drop"
	)

#### analyse F0/pitch ----------------------------------------------------------
pitch <- extract_pitch(file = here("Stimuli", "Audios")) %>%
	mutate(file = paste0(file, ".wav")) %>% 
	left_join(trials) 

pitch_mean <- pitch %>%
	group_by(audio, test_language, trial_type) %>%
	summarise(
		f0 = mean(f0, na.rm = TRUE),
		.groups = "drop"
	)

#### merge data ----------------------------------------------------------------
merged <- trials %>%
	left_join(amplitude_means) %>%
	left_join(pitch_mean) 

#### export data ---------------------------------------------------------------
saveRDS(merged, here("Results", "stimuli.rds"))

#### visualise data ------------------------------------------------------------
# intensity
plot_intensity <- ggplot(drop_na(merged), aes(trial_type, intensity, fill = trial_type, colour = trial_type)) +
	facet_wrap(~test_language) +
	stat_slab(position = position_nudge(x = 0.3)) + 
	geom_boxplot(width = 0.1, colour = "black", fill = "white", position = position_nudge(x = 0.3)) +
	geom_jitter(width = 0.05, stroke = 0, alpha = 0.25) +
	labs(x = "Language", y = "Intensity (dB)", colour = "Language", fill = "Language") +
	scale_color_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	theme_custom() +
	theme(legend.position = "none", axis.title.x = element_blank())

# duration
plot_duration <- ggplot(drop_na(merged), aes(trial_type, duration, fill = trial_type, colour = trial_type)) +
	facet_wrap(~test_language) +
	stat_slab(position = position_nudge(x = 0.3)) + 
	geom_boxplot(width = 0.1, colour = "black", fill = "white", position = position_nudge(x = 0.3)) +
	geom_jitter(width = 0.05, stroke = 0, alpha = 0.25) +
	labs(x = "Language", y = "Duration (s)", colour = "Language", fill = "Language") +
	scale_color_brewer(palette = "Dark2") +
	scale_fill_brewer(palette = "Dark2") +
	theme_custom() +
	theme(legend.position = "none", axis.title.x = element_blank())

# arrange plots together
(plot_duration + plot_intensity) +
	plot_layout(ncol = 1) +
	ggsave(here("Figures", "stimuli.png"), height = 10, width = 16)


# individual plots -------------------------------------------------------------
if (individual_plots) {
	# amplitude/intensity
	amplitude %>%
		split(.$soundfile) %>%
		map(~ggplot(., aes(time, intensity)) +
				geom_line() +
				labs(x = "Time", y = "Intensity (dB)",
					 title = str_to_upper(distinct(., orthography)),
					 subtitle = paste0("Language: ", distinct(., language),
					 				  " / Phonology (IPA): ", distinct(., phonology))) +
				theme(panel.background = element_rect(fill = "transparent"),
					  panel.border = element_rect(fill = "transparent", colour = "black"),
					  panel.grid = element_blank(),
					  text = element_text(size = 12),
					  axis.text = element_text(colour = "black"),
					  legend.position = c(0.1, 0.9),
					  legend.direction = "horizontal") +
				ggsave(here("Figures",
							paste0(
								str_replace(unique(.$soundfile), ".wav", ".png") %>% str_replace(., "Sounds", "Amplitude"))),
							width = 6, height = 5)
		)
}



