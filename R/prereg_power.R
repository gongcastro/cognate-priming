# prereg_power: Power analysis for preregistration
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Congition, Universitat Pompeu Fabra

#### set up ##############################################

# load packages
library(dplyr)      # for manipulating data
library(tibble)     # for creating data frames
library(purrr)      # for working with lists
library(truncnorm)  # for sampling a truncated normal distribution
library(lme4)       # for Linear Mixed Models

#### set parameters ######################################
set.seed(888) # set seed for reproducibility

# number of time bins in each trial
sampling_rate        <- 120
time_window_duration <- 2000
timebin_duration     <- 100
n_timebins           <- time_window_duration/timebin_duration
n_age                <- 3
n_participants       <- 20*2*2
n_trials             <- 32

# model parameters
mu_l    <- 10  # mean intercent/initial status
mu_s    <- 0.5 # mean slope rate of change
sigma_l <- 2   # intercept standard deviation
sigma_s <- 1   # slope standard deviation
rho     <- 0   # correlation parameter between intercepts and slopes
miss    <- 0   # missing data rate, 0: no missing data

# power parameters
n_sim   <- 10         # number of simulations to perform
t       <- n_timebins # number of measurements along time domain
n_start <- 15         # minimum sample size to consider
n_end   <- 25         # maximum sample siz to consider


step    <- 2           # step between two sample sizes

# means
means <- tribble(
	~Age, ~Languages       , ~LangProfile , ~TrialType   , ~RealPTL,
	20  , "Spanish-English", "Monolingual", "Unrelated"  , 65.0,
	20  , "Spanish-English", "Monolingual", "Non-cognate", 60.0,
	20  , "Spanish-English", "Monolingual", "Cognate"    , 60.0,
	20  , "Spanish-English", "Bilingual"  , "Unrelated"  , 65.0,
	20  , "Spanish-English", "Bilingual"  , "Non-cognate", 60.0,
	20  , "Spanish-English", "Bilingual"  , "Cognate"    , 55.0,
	24  , "Spanish-English", "Monolingual", "Unrelated"  , 70.0,
	24  , "Spanish-English", "Monolingual", "Non-cognate", 62.5,
	24  , "Spanish-English", "Monolingual", "Cognate"    , 62.5,
	24  , "Spanish-English", "Bilingual"  , "Unrelated"  , 70.0,
	24  , "Spanish-English", "Bilingual"  , "Non-cognate", 62.5,
	24  , "Spanish-English", "Bilingual"  , "Cognate"    , 55.0,
	30  , "Spanish-English", "Monolingual", "Unrelated"  , 75.0,
	30  , "Spanish-English", "Monolingual", "Non-cognate", 65.0,
	30  , "Spanish-English", "Monolingual", "Cognate"    , 62.5,
	30  , "Spanish-English", "Bilingual"  , "Unrelated"  , 75.0,
	30  , "Spanish-English", "Bilingual"  , "Non-cognate", 65.0,
	30  , "Spanish-English", "Bilingual"  , "Cognate"    , 55.0, 
	20  , "Spanish-Catalan", "Monolingual", "Unrelated"  , 65.0,
	20  , "Spanish-Catalan", "Monolingual", "Non-cognate", 60.0,
	20  , "Spanish-Catalan", "Monolingual", "Cognate"    , 60.0,
	20  , "Spanish-Catalan", "Bilingual"  , "Unrelated"  , 65.0,
	20  , "Spanish-Catalan", "Bilingual"  , "Non-cognate", 60.0,
	20  , "Spanish-Catalan", "Bilingual"  , "Cognate"    , 55.0,
	24  , "Spanish-Catalan", "Monolingual", "Unrelated"  , 70.0,
	24  , "Spanish-Catalan", "Monolingual", "Non-cognate", 62.5,
	24  , "Spanish-Catalan", "Monolingual", "Cognate"    , 62.5,
	24  , "Spanish-Catalan", "Bilingual"  , "Unrelated"  , 70.0,
	24  , "Spanish-Catalan", "Bilingual"  , "Non-cognate", 62.5,
	24  , "Spanish-Catalan", "Bilingual"  , "Cognate"    , 55.0,
	30  , "Spanish-Catalan", "Monolingual", "Unrelated"  , 75.0,
	30  , "Spanish-Catalan", "Monolingual", "Non-cognate", 65.0,
	30  , "Spanish-Catalan", "Monolingual", "Cognate"    , 62.5,
	30  , "Spanish-Catalan", "Bilingual"  , "Unrelated"  , 75.0,
	30  , "Spanish-Catalan", "Bilingual"  , "Non-cognate", 65.0,
	30  , "Spanish-Catalan", "Bilingual"  , "Cognate"    , 55.0 
)

#### simulate raw data #########################################################

data <- list() # pre-allocate data frame
nrows <- n_timebins*n_participants*n_trials*n_age

data <- map(
	paste0("sim", 1:10),
	~data.frame(
		# simulation ID
		Sim = .,
		# wihtin variables
		ParticipantID = rep(1:n_participants, each = n_timebins*n_age*n_trials),
		TrialID = rep(1:n_trials, each = n_timebins, times = n_participants*n_age),
		# within variables
		TimeBin = rep(1:n_timebins, times = n_participants*n_age*n_trials),
		Age = rep(1:n_age, each = n_trials*n_timebins),
		TrialType = rep(0:3, each  = 8*n_timebins, times = n_trials*n_age*n_participants),
		# between variables
		LangProfile = rep(0:1, each = n_timebins*n_trials*n_age, times = n_participants/2),
		Languages = rep(0:1, each = n_timebins*n_trials*n_age*2, times = n_participants/4)
		)
	) %>%
	bind_rows() 
	map(~left_join(., means, by = c("Age", "TrialType", "LangProfile", "Languages"))) 
	map(~mutate(., PTL = rtruncnorm(n = 1, a = 0, b = 1, mean = RealPTL/100, sd = 0.07)))

#### fit model #############################################################
fit <- map(data,
		   ~lmer(formula = PTL ~ Age*TrialType*Languages*LangProfile +
		   	  	(1 + Age + TrialType | Participant),
		   	 data = .,
		   	 REML = FALSE,
		   	 control = lmerControl(optimizer = "bobyqa")
		   )
)

fixeffect <- fit %>%
	map("")



