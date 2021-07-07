# update project
source("R/00_stimuli.R")
source("R/01_participants.R")
source("R/02_vocabulary.R")
source("R/03_gaze_barcelona.R")
source("R/03_gaze_oxford.R")
source("R/04_attrition.R")
source("R/05_analysis.R")
rmarkdown::render("Rmd/report.rmd")
