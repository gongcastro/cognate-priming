#### filter_trials_phase: Filter trials by trial phase
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Universitat Pompeu Fabra

################################################################

filter_trial_phase <- function(
	data,
	participant_col,
	trial_phase_col,
	trial_phase = NULL
	
){
	
	# load packages
	require(rlang, warn.conflicts = FALSE, quietly = TRUE)
	require(dplyr, warn.conflicts = FALSE, quietly = TRUE)
	require(tidyr, warn.conflicts = FALSE, quietly = TRUE)
	
	# filter data
	data.phase <- if(!is.null({{ trial_phase }}))
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
	
	
	
	assign("data", data, envir = .GlobalEnv)
	assign("data.phase", data.phase, envir = .GlobalEnv)
	
	
}
