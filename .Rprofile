source("renv/activate.R")

# if targets package is not installed, install it
if ("targets" %in% utils::installed.packages()) library(targets)

# if targets package is not installed, install it
if ("conflicted" %in% utils::installed.packages()) library(conflicted)

# load wrapper functions
source("R/utils.R")

# load packages
library(targets)
library(conflicted)

conflict_prefer("last_warnings", "rlang")
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("timestamp", "utils")
conflict_prefer("ar", "brms")
conflict_prefer("chisq.test", "stats")
conflict_prefer("discard", "scales")
conflict_prefer("duration", "lubridate")
conflict_prefer("fisher.test", "stats")
conflict_prefer("lag", "dplyr")

usethis::ui_info('Welcome! To execute the repository, 1) run renv::restore(), 2) and then make() in your R console.')
usethis::ui_info('Check https://github.com/gongcastro/cognate-priming for instructions, documentation, and troubleshooting.')

options(knitr.duplicate.label = "allow")
