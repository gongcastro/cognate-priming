library(shiny)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(
    # Application title
    
    navbarPage(
        "Cognate Priming",
        tabPanel("Summary", summaryUI()),
        tabPanel("Participants", participantsUI()),
        tabPanel("Vocabulary", vocabularyUI()),
        tabPanel("Stimuli", stimuliUI()),
        tabPanel("Gaze data", gazeUI()),
        tabPanel("Model outputs", modelUI()),
        navbarMenu("More", tabPanel("Documentation"), tabPanel("Code"))
    )
)