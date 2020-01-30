# 02_gaze: Analyse gaze in Cognate Priming task
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ############################################

# load packages
library(magrittr)     # for using pipes
library(here)         # for locating files
library(tibble)       # for tidy data presentation
library(dplyr)        # for manipulating data
library(tidyr)        # for rehsaping datasets
library(purrr)        # for working with lists
library(eyetrackingR) # for processing eye-tracking data
library(ggplot2)      # for data visualisation
library(patchwork)    # for arranging plots

# set parameters
time_bin_duration <- 50
screenX           <- 1920
screenY           <- 1080

#### import data #########################################################
data <- read.table(here::here("Data", "02_filtered.txt"), sep = "\t", header = TRUE) %>%
  as_tibble() %>%
  mutate(trialType = factor(trialType, levels = c("unrelated", "noncognate", "cognate")),
         profile = case_when(spanish > 0.80 ~ "monolingual",
                             catalan > 0.80 ~ "monolingual",
                             TRUE       ~ "bilingual")) %>%
  make_eyetrackingr_data(data               = .,
                         participant_column = "ID",
                         trackloss_column   = "meanValidity",
                         time_column        = "time",
                         trial_column       = "Trial",
                         aoi_columns        = c("gazeT", "gazeD"),
                         item_columns       = "trialType",
                         treat_non_aoi_looks_as_missing = TRUE) %>%
  subset_by_window(rezero = TRUE,
                   remove = TRUE,
                   window_start_time = 0,
                   window_end_time = 2000)

#### reconstruct gaze ####################################################
data %>%
  mutate(OnsetSection = case_when(between(time, 0, 100)   ~ "0-100 ms",
                                  between(time, 100, 200) ~ "100-200 ms",
                                  between(time, 200, 300) ~ "200-300 ms",
                                  between(time, 300, 400) ~ "300-400 ms",
                                  between(time, 400, 500) ~ "400-500 ms",
                                  TRUE                    ~ "Later")) %>%
  ggplot(aes(x = meanX, y = meanY)) +
  facet_wrap(~OnsetSection) +
  geom_rect(aes(xmin = 0, xmax = screenX, ymin = 0, ymax = screenY),
            fill = "#7F7F7F", colour = "black") +
  geom_bin2d() +
  geom_rect(aes(xmin = 710, xmax = 1210, ymin = 290, ymax = 790),
            fill = "transparent", colour = "white") +
  labs(x = "Gaze position in X axis (px)", y = "Gaze coordinates in Y axis (px)",
       fill = "Count") +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(limits = c(0, screenX), breaks = c(0, screenX)) +
  scale_y_continuous(limits = c(0, screenY), breaks = c(0, screenY)) +
  coord_fixed() +
  theme(
    panel.background = element_rect(fill = "transparent")
  ) +
  ggsave(here::here("Figures", "03_gaze-reconstruction.png"), height = 4, width = 7)

#### summary #############################################################
summaryT <- describe_data(data,
                          describe_column = c("gazeT"),
                          group_columns   = c("ID", "trialType", "language", "profile")) %>%
  mutate(item = "Target",
         Mean = Mean*100,
         SD   = SD*100)
summaryD <- describe_data(data,
                          describe_column = c("gazeD"),
                          group_columns   = c("ID", "trialType", "language", "profile")) %>%
  mutate(item = "Distractor",
         Mean = Mean*100,
         SD   = SD*100)

summary <- rbind(summaryT, summaryD)

summary.stats <- summary %>% 
  mutate(item = factor(item, levels = c("Target", "Distractor"))) %>%
  group_by(trialType, item, profile, language) %>%
  summarise(mean = mean(Mean, na.rm = TRUE),
            sd = sd(Mean, na.rm = TRUE),
            n = n(),
            sem = sd/sqrt(n))

# overall summary plot
ggplot(summary, aes(item, Mean)) +
  facet_wrap(~trialType) +
  geom_hline(yintercept = 50) +
  geom_line(aes(group = ID), colour = "grey", size = 0.5, alpha = 0.5) +
  geom_point(aes(colour = trialType), size = 2, alpha = 0.5) +
  stat_summary(aes(group = trialType), fun.y = mean, geom = "line", colour = "black") +
  stat_summary(fun.data = mean_se, geom = "pointrange", colour = "black") +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(y = "Samples on AOI (%)", colour = "Trial type",
       title = "Samples on target and distractor", subtitle = "Mean and SEM") +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_line(linetype = "dotted", colour = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = "transparent"),
    legend.position = "top"
  ) +
  ggsave(here::here("Figures", "03_summary-gaze-overall.png"))


# group summary plot
ggplot(summary, aes(item, Mean)) +
  facet_grid(profile~language~trialType) +
  geom_hline(yintercept = 50) +
  geom_line(aes(group = ID), colour = "grey", size = 0.5, alpha = 0.5) +
  geom_point(aes(colour = trialType), size = 2) +
  stat_summary(fun.data = mean_se, geom = "pointrange", colour = "black",
               size = 0.5, position = position_nudge(x = -0.25)) +
  geom_label(data = summary.stats,
             aes(x = item, y = 90, label = paste(round(mean, 2), "%"))) +
  scale_colour_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "Samples on AOI (%)", colour = "Trial type",
       title = "Samples on target and distractor", subtitle = "Mean and SEM") +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_line(linetype = "dotted", colour = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = "transparent"),
    legend.position = "top"
  ) +
  ggsave(here::here("Figures", "03_summary-gaze.png"), height = 8)

# by-participant summary plot
ggplot(summary, aes(x = item, y = Mean, ymin = Mean-SD, ymax = Mean+SD,
         colour = trialType)) +
  facet_wrap(ID~profile~trialType, scales = "free_y", ncol = 3) +
  geom_hline(yintercept = 50) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.25, size = 0.75, position = position_dodge(width = 0.5)) +
  labs(y = "Target looking (%)", colour = "Trial type",
       title = "Proportion of target looking time by participant",
       subtitle = "Mean and SD") +
  scale_colour_brewer(palette = "Dark2") +
  theme(
    axis.text.x = element_text(colour = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_line(linetype = "dotted", colour = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(colour = "black", fill = "transparent"),
    legend.position = "none"
  ) +
  ggsave(here::here("Figures", "03_summary-gaze-participants.png"), height = 20)

#### time bins ###########################################################
data.bins <- data %>%
  make_time_sequence_data(time_bin_size = time_bin_duration,
                          aois = c("gazeT"),
                          predictor_columns = c("trialType", "profile", "language")) %>%
  as_tibble() %>%
  ungroup() %>%
  group_by(profile, language, ID, TimeBin, trialType) %>%
  summarise(SamplesInAOI = sum(SamplesInAOI, na.rm = TRUE),
            SamplesTotal = sum(SamplesTotal, na.rm = TRUE)) %>%
  group_by(profile, TimeBin, trialType, language) %>%
  ungroup() %>%
  mutate(Prop = 100*SamplesInAOI/SamplesTotal,
         Time = TimeBin*time_bin_duration)

ggplot(data.bins, aes(x = Time, y = Prop, colour = trialType, fill = trialType)) +
  facet_wrap(~trialType) +
  geom_hline(yintercept = 50) +
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha  = 0.25, colour = NA) +
  stat_summary(fun.y = mean, geom = "line") +
  labs(x = "Time (ms)", y = "Samples on target (%)",
       colour = "Trial type", fill = "Trial type",
       title = "Samples in target over time", subtitle = "Mean and SEM") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 2000),
                     breaks = seq(0, 2000, 500)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    axis.text.x = element_text(colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid = element_line(linetype = "dotted", colour = "grey"),
    panel.border = element_rect(colour = "black", fill = "transparent"),
    legend.position = "top",
  ) +
  ggsave(here::here("Figures", "04_gaze-time.png"))

#### export data ##################################################
write.table(data, here("Data", "03_gaze-raw.txt"), sep = "\t")
write.table(data.bins, here("Data", "03_gaze-timebins.txt"), sep = "\t")
write.table(summary, here("Data", "03_summary.txt"), sep = "\t")

