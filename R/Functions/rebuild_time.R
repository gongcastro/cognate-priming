# rebuild_time: Rebuild time domain (in ms) from n samples and sampling rate

###################################################################
rebuild_time <- function(
	n_samples,
	sampling_rate
){
	# load packages
	require(rlang)
	
	seq(from = 0, to = {{ n_samples }}, by = 1000/{{ sampling_rate }})
}
