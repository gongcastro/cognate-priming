#### participants: analyse samples----------------------------------------------

#### set up --------------------------------------------------------------------

# load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(here)

#### import data ---------------------------------------------------------------

d <- read_xlsx(here("Data", "participants.xlsx")) %>% 
	filter(location=="Barcelona",
		   !pilot,
		   valid_participant
	) %>% 
	mutate_at(vars(age_group, list), as.factor) %>%
	group_by(age_group, test_language, lp, list, .drop = FALSE) %>% 
	summarise(n = n(), .groups = "drop") %>% 
	right_join(
		expand(age_group, test_language, lp, list),
		by = c("age_group", "test_language", "lp", "list")
	) %>% 
	mutate(
		n = ifelse(is.na(n), 0, n),
		age_group = paste0(age_group, " months")
	) %>% 
	arrange(age_group, test_language, lp, list)

ggplot(d, aes(age_group, n, fill = list, label = n)) +
	facet_grid(test_language~lp, drop = FALSE) +
	geom_col() +
	geom_label(position = position_stack()) +
	labs(x = "Age group", y = "N", fill = "List") +
	theme_bw() +
	theme(axis.title= element_blank(),
		  axis.text = element_text(colour = "black"),
		  panel.grid.major.x = element_blank(),
		  legend.position = "top") +
	ggsave(here("Figures", "participants.png"))
