#### filter_trials_aoi: Filter trials according by time in AOI
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

################################################################

filter_trials <- function(
	
	data,                                          # dataset
	participant_cols,                              # column indexing participants
	aoi_cols,                                      # column(s) evaluating whether gaze is in AOIs
	trial_cols,                                    # column indexing trials
	condition_cols,                                # column indexing gruping variables
	trackloss_cols,                                # column indicating trackloss
	trial_phase_col,                               # column indicating trial phase
	trial_phase                            = NULL, # trial phase(es) of interest; if NULL include all
	trial_aoi                              = NULL, # AOI; if NULL include all
	max_trackloss_trial                    = 0.75, # max number of invalid samples by trial
	max_trackloss_trial_by_phase           = TRUE, # should max_trackloss_trial be applied phase-wise?
	max_trackloss_participant              = 0.50, # max number of invalid trials by participant
	max_trackloss_participant_by_condition = TRUE, # should max_trackloss_participant by applied condition-wise? 
	min_samples_aoi                        = 0.10, # min n samples to consider participant has looked at AOI
	min_samples_aoi_each                   = TRUE  # should min_samples_aoi be applied AOI-wise?
	
	
){
	
	# load packages
	require(rlang, warn.conflicts = FALSE, quietly = TRUE)
	require(dplyr, warn.conflicts = FALSE, quietly = TRUE)
	require(tidyr, warn.conflicts = FALSE, quietly = TRUE)
	
	# filter by phase
	data.phase <-
		if(!is.null({{ trial_phase }}))
			filter({{ data }}, {{ trial_phase_col }}=={{ trial_phase }})
	else {{ data }}
	
	# print log
	message(
		paste0("Observations not correponding to the ",
			   {{ trial_phase }},
			   " trial phase were removed: ",
			   nrow({{ data }})-nrow(data.phase),
			   " out of ",
			   nrow({{ data }}),
			   " (", 100*(nrow(data.phase)/nrow({{ data }})), "%)",
			   " remaining."
		)
	)
	
	# remove with too few valid samples on AOI
	data.n.aoi.grouped <-
		if(!is.null({{ min_samples_aoi_each }}))
			data.phase %>%
		group_by_at(vars(one_of({{ participant_cols }}, {{ trial_cols }}))) %>%
		summarise_at(vars({{ aoi_cols }}, sum, na.rm = TRUE)) %>%
		ungroup()
	else
		data.phase
	
		
		
		
	assign("data", data, envir = .GlobalEnv)
	assign("data.phase", data.phase, envir = .GlobalEnv)
	assign("data.aoi", data.n.aoi.grouped, envir = .GlobalEnv)
	
	
}
