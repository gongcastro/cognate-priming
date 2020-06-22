#### fixations2
library(tidyverse)
library(patchwork)
library(data.table)
library(here)

screen_x <- 1920
screen_y <- 1080

l_coords <- c(xmin = 1920/4-500/2, xmax = 1920/4+500/2, ymin = 1080/2-500/2, ymax = 1080/2+500/2)
r_coords <- c(xmin = 1920-1920/4-500/2, xmax = 1920-1920/4+500/2, ymin = 1080/2-500/2, ymax = 1080/2+500/2)

dat <- fread(here("Data", "03_fixations.csv"), sep = ",", dec = ".", stringsAsFactors = FALSE) %>%
	as_tibble()

d <- dat %>%
	mutate(., fixation = ifelse(is.na(fixation), FALSE, fixation)) %>%
	group_split(participant_id) 

# heatmap
for (i in 1:length(d)){
	d[[i]] %>%
	filter(time >= 300) %>%
	ggplot(aes(x, y, colour = time)) +
	facet_wrap(~trial_id) +
	annotate("rect", xmin = l_coords[1], xmax = l_coords[2], ymin = l_coords[3], ymax = l_coords[4],
			 colour = NA, fill = "white", alpha = 0.1) +
	annotate("rect", xmin = r_coords[1], xmax = r_coords[2], ymin = r_coords[3], ymax = r_coords[4],
			 colour = NA, fill = "white", alpha = 0.1) +
	geom_text(aes(x = 200, y = 1000, label = paste0("Target: ", target_location)), colour = "white", size = 2) +
	geom_point(alpha = 0.25, size = 0.1) +
	labs(colour = "Time (ms)",
		 title = paste0("Participant: ", unique(d[[i]]$participant_id)),
		 subtitle = paste0(unique(d[[i]]$lang_profile), ", ", unique(d[[i]]$language), ", list ", unique(d[[1]]$list))) +
	theme(legend.position = "top",
		  axis.text = element_blank(),
		  axis.title = element_blank(),
		  axis.ticks = element_blank(),
		  strip.text = element_text(size = 7),
		  panel.background = element_rect(fill = "#808080"),
		  panel.grid = element_blank(),
		  panel.border = element_rect(colour = "grey", fill = "transparent")) +
	coord_fixed(xlim = c(0, screen_x), ylim = c(0, screen_y)) +
	ggsave(here("Figures", "Fixations", paste0(unique(d[[i]]$participant_id), "_gaze.png")), height = 7)
}

# fixations
for (i in 1:length(d)){
	d[[i]] %>%
		filter(time >= 300) %>%
		pivot_longer(c(x, y), names_to = "measure", values_to = "value") %>%
		ggplot(aes(time, value, colour = fixation, shape = measure)) +
		facet_wrap(~trial_id) +
		geom_point(size = 0.25, alpha = 0.5) +
		labs(x = "Time (ms)",
			 y = "Gaze position",
			 alpha = "Fixation",
			 colour = "Coordinate",
			 title = paste0("Participant: ", unique(d[[i]]$participant_id)),
			 subtitle = paste0(unique(d[[i]]$lang_profile), ", ", unique(d[[i]]$language), ", list ", unique(d[[1]]$list))) +
		theme(legend.position = "top",
			  strip.text = element_text(size = 7),
			  panel.background = element_rect(fill = "transparent"),
			  panel.grid = element_line(colour = "grey80", linetype = "solid", size = 0.1),
			  panel.border = element_rect(colour = "grey", fill = "transparent")) +
		ggsave(here("Figures", "Fixations", paste0(unique(d[[i]]$participant_id), "_fixations.png")), width = 8, height= 5)
}
