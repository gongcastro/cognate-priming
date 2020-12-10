#### utils: helper functions ---------------------------------------------------

# evaluate if x is NOT an element of y
"%!in%" <- function(x, y){!(x %in% y)}

# evaluate if gaze is in distractor AOI
eval_distractor <- function(
	data,
	x_gaze,
	y_gaze,
	target_location,
	l_coords = c(280, 780, 290, 790),
	r_coords = c(1140, 1640, 290, 790)
){
	data %>%
		mutate(
			d_xmin = case_when(target_location== "l" ~ r_coords[1],
							   target_location=="r" ~ l_coords[1],
							   TRUE ~ NA_real_),
			d_xmax = case_when(target_location== "l" ~ r_coords[2],
							   target_location=="r" ~ l_coords[2],
							   TRUE ~ NA_real_),
			d_ymin = case_when(target_location=="l" ~ r_coords[3],
							   target_location=="r" ~ l_coords[3],
							   TRUE ~ NA_real_),
			d_ymax = case_when(target_location=="l" ~ r_coords[4],
							   target_location=="r" ~ l_coords[4],
							   TRUE ~ NA_real_),
			gazeD = ((x_gaze>= d_xmin) & (x_gaze<=d_xmax) & (y_gaze>=d_ymin) & (y_gaze<=d_ymax))
		) %>%
		pull(gazeD)
}

# evaluate if gaze is in prime
eval_prime <- function(
	data,
	x_gaze,
	y_gaze,
	p_coords = c(710, 1210, 290, 790)
){
	# load packages
	require(rlang, quietly = TRUE, warn.conflicts = FALSE)
	require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
	require(tidyr, quietly = TRUE, warn.conflicts = FALSE)
	
	{{ data }} %>%
		mutate(
			p_xmin = p_coords[1],
			p_xmax = p_coords[2],
			p_ymin = p_coords[3],
			p_ymax = p_coords[4],
			gazeP = (({{x_gaze}} >= p_xmin) & ({{x_gaze}} <= p_xmax) & ({{y_gaze}} >= p_ymin) & ({{y_gaze}} <= p_ymax)),
		) %>%
		pull(gazeP)
}

eval_target <- function(
	data,
	x_gaze,
	y_gaze,
	target_location,
	l_coords = c(280, 780, 290, 790),
	r_coords = c(1140, 1640, 290, 790)){
	
	data %>%
		mutate(
			t_xmin = case_when(target_location=="r" ~ r_coords[1],
							   target_location=="l" ~ l_coords[1],
							   TRUE ~ NA_real_),
			t_xmax = case_when(target_location=="r" ~ r_coords[2],
							   target_location=="l" ~ l_coords[2],
							   TRUE ~ NA_real_),
			t_ymin = case_when(target_location=="r" ~ r_coords[3],
							   target_location=="l" ~ l_coords[3],
							   TRUE ~ NA_real_),
			t_ymax = case_when(target_location=="r" ~ r_coords[4],
							   target_location=="l" ~ l_coords[4],
							   TRUE ~ NA_real_),
			gazeT = (x_gaze>=t_xmin) & (x_gaze<=t_xmax) & (y_gaze>=t_ymin) & (y_gaze<=t_ymax),
		) %>%
		pull(gazeT)
}


