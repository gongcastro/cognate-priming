library(here)
library(dplyr)
library(shiny)
library(tidybayes)
library(ggplot2)
library(purrr)
library(brms)
library(scales)

fit <- readRDS(here("Results", "fit.rds"))
d <- fit$data

source(here("R", "utils.R"))

# population-level effects
message("Generating population-level effects...")

coefs <- gather_draws(fit, `b_.*`, regex = TRUE)

nd <- expand.grid(
    age_group = paste0(c(21, 25, 30), " months"),
    lp = c("Monolingual", "Bilingual"),
    trial_type = unique(d$trial_type),
    time_bin_center = seq(min(d$time_bin_center), max(d$time_bin_center), 0.25)
)

preds <- add_epred_draws(nd, fit, ndraws = 25, re_formula = NA) %>% 
    mutate(.epred = inv_logit_scaled(.epred))


# conditional means by participant
message("Generating participant-level effects...")

nd_participant <- expand.grid(
    participant = unique(d$participant),
    trial_type = unique(d$trial_type),
    time_bin_center = seq(min(d$time_bin_center), max(d$time_bin_center), 0.25)
) %>% 
    left_join(distinct(d, participant, age_group, lp))

preds_participant <- add_epred_draws(
    nd_participant, fit, ndraws = 25,
    re_formula = ~ (1 + time_bin_center*trial_type + age_group) | participant
) %>% 
    mutate(.epred = inv_logit_scaled(.epred)) %>% 
    ungroup() %>% 
    group_split(participant) %>% 
    set_names(unique(d$participant))

coefs_participant <- gather_draws(fit, r_participant[participant, param]) %>% 
    ungroup() %>% 
    group_split(participant) %>% 
    set_names(unique(d$participant))

# conditional means by target
message("Generating target-level effects...")

nd_target <- expand.grid(
    target = unique(d$target),
    age_group = paste0(c(21, 25, 30), " months"),
    lp = c("Monolingual", "Bilingual"),
    trial_type = unique(d$trial_type),
    time_bin_center = seq(min(d$time_bin_center), max(d$time_bin_center), 0.25)
)

preds_target <- add_epred_draws(
    nd_target, fit, ndraws = 25,
    re_formula = ~ (1 + time_bin_center*trial_type*lp + age_group) | target
) %>% 
    mutate(.epred = inv_logit_scaled(.epred)) %>% 
    ungroup() %>% 
    group_split(target) %>% 
    set_names(unique(d$target))

coefs_target <- gather_draws(fit, r_target[target, param]) %>% 
    ungroup() %>% 
    group_split(target) %>% 
    set_names(unique(d$target))

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Exploring the model posterior",
    tabPanel(
        "Population-level effects",
        column(width = 6, plotOutput("plot_coefs")),
        column(width = 6, plotOutput("plot_preds"))
    ),
    navbarMenu(
        "Group-level effects",
        tabPanel(
            "Participant-level estimates",
            column(width = 2, selectInput("participant", "Participant", choices = unique(d$participant))),
            column(width = 4, plotOutput("plot_coefs_participant")),
            column(width = 4, plotOutput("plot_preds_participant"))
        ),
        tabPanel(
            "Target-level estimates",
            column(width = 2, selectInput("target", "Target", choices = unique(d$target))),
            column(width = 4, plotOutput("plot_coefs_target")),
            column(width = 4, plotOutput("plot_preds_target"))
        )
        
    )
)


server <- function(input, output) {
    
    # plot for population-level marginal means
    output$plot_preds <- renderPlot({
        preds %>% 
            ggplot(aes(time_bin_center, .epred, colour = trial_type)) +
            facet_grid(lp~age_group, drop = FALSE) +
            # stat_summary(data = d %>% filter(participant == "cognatepriming70"),
            # 		   aes(y = inv_logit_scaled(logit_adjusted)),
            # 			 fun.data = mean_se, geom = "pointrange", size = 0.25) +
            geom_line(aes(group = interaction(.draw, trial_type)), alpha = 0.75, size = 0.5) +
            #stat_summary(fun = mean, geom = "line", size = 1.5) +
            labs(x = "Time bin (100 ms)", y = "Posterior probabiliy of \n target fixation", 
                 colour = "Condition") +
            scale_color_brewer(palette = "Set1") +
            scale_y_continuous(labels = percent, limits = c(0, 1)) +
            scale_x_continuous(labels = 1:length(unique(d$time_bin_center)),
                               breaks = unique(d$time_bin_center)) +
            theme_ggdist() +
            theme(
                legend.position = "top",
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    # plot for population-level coefs
    output$plot_coefs <- renderPlot({
        coefs %>% 
            ggplot(aes(.value, .variable)) + 
            geom_vline(xintercept = 0, colour = "grey", size = 1) +
            stat_pointinterval() +
            labs(x = "Posterior", y = "Variable") +
            theme_ggdist() +
            theme(
                legend.position = "top",
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    
    # plot for participant-level conditional means
    output$plot_preds_participant <- renderPlot({
        preds_participant[[input$participant]] %>% 
            ggplot(aes(time_bin_center, .epred, colour = trial_type)) +
            facet_grid(~age_group, drop = FALSE) +
            # stat_summary(data = d %>% filter(participant == "cognatepriming70"),
            # 		   aes(y = inv_logit_scaled(logit_adjusted)),
            # 			 fun.data = mean_se, geom = "pointrange", size = 0.25) +
            geom_line(aes(group = interaction(.draw, trial_type)), alpha = 0.75, size = 0.5) +
            #stat_summary(fun = mean, geom = "line", size = 1.5) +
            labs(x = "Time bin (100 ms)", y = "Posterior probabiliy of \n target fixation", 
                 colour = "Condition") +
            scale_color_brewer(palette = "Set1") +
            scale_y_continuous(labels = percent, limits = c(0, 1)) +
            scale_x_continuous(labels = 1:length(unique(d$time_bin_center)),
                               breaks = unique(d$time_bin_center)) +
            theme_ggdist() +
            theme(
                legend.position = "top",
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    output$plot_coefs_participant <- renderPlot({
        coefs_participant[[input$participant]] %>% 
            ggplot(aes(.value, param)) +
            geom_vline(xintercept = 0, colour = "grey", size = 1) +
            stat_pointinterval() +
            labs(x = "Posterior", y = "Variable") +
            theme_ggdist() +
            theme(
                legend.position = "top",
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    # plot for target-level conditional means
    output$plot_preds_target <- renderPlot({
        preds_target[[input$target]] %>% 
            ggplot(aes(time_bin_center, .epred, colour = trial_type)) +
            facet_grid(lp~age_group, drop = FALSE) +
            # stat_summary(data = d %>% filter(participant == "cognatepriming70"),
            # 		   aes(y = inv_logit_scaled(logit_adjusted)),
            # 			 fun.data = mean_se, geom = "pointrange", size = 0.25) +
            geom_line(aes(group = interaction(.draw, trial_type)), alpha = 0.75, size = 0.5) +
            #stat_summary(fun = mean, geom = "line", size = 1.5) +
            labs(x = "Time bin (100 ms)", y = "Posterior probabiliy of \n target fixation", 
                 colour = "Condition") +
            scale_color_brewer(palette = "Set1") +
            scale_y_continuous(labels = percent, limits = c(0, 1)) +
            scale_x_continuous(labels = 1:length(unique(d$time_bin_center)),
                               breaks = unique(d$time_bin_center)) +
            theme_ggdist() +
            theme(
                legend.position = "top",
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
    
    output$plot_coefs_target <- renderPlot({
        coefs_target[[input$target]] %>% 
            ggplot(aes(.value, param)) + 
            geom_vline(xintercept = 0, colour = "grey", size = 1) +
            stat_pointinterval() +
            labs(x = "Posterior", y = "Variable") +
            theme_ggdist() +
            theme(
                legend.position = "top",
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
