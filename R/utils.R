# eyetracking utils ------------------------------------------------------------

# get name dictionary 
get_name_dictionary <- function(...) {
	
	col_name_changes <- c("participant" = "id",
						  "system_time_stamp" = "time",
						  "l_1" = "l_x",
						  "l_2" = "l_y",
						  "process" = "phase",
						  "r_1" = "r_x",
						  "r_2" = "r_y",
						  "l_user_coord_3" = "l_user_coord_z",
						  "r_user_coord_3" = "r_user_coord_z",
						  "suje_num" = "id",
						  "numtrial_lista" = "trial_num",
						  "trial_num" = "trial_id")
	
	phase_name_changes <- c("GETTER" = "Getter",
							"PRIMEIMAGE" = "Prime",
							"TARGET_DISTRACTOR" = "Target-Distractor",
							"prime" = "Prime",
							"primeimage" = "Prime",
							"target_distractor" = "Target-Distractor")
	
	relevant_variables <- c("id", "trial", "trial_id", "phase", "x", "y", "valid_sample")
	
	name_dict <- lst(col_name_changes, 
					 phase_name_changes, 
					 relevant_variables)
	
	return(name_dict)
}

fix_timestamps <- function(x) {
	x_cols <- select(x, l_x, r_x, l_y, r_y)
	is_any_char <- any(map_chr(x_cols, class)=="character")
	if (is_any_char) {
		x <- mutate(
			x,
			across(
				c(l_x, r_x, l_y, r_y),
				function(x) {
					out <- gsub("\\.", "", x)
					out <- gsub("^(.{1})(.*)$", "\\1.\\2", out,
								perl = TRUE)
					out <- as.double(out)
					return(out)
				}
			)
		)
	}
	return(x)
}

fix_validity <- function(x) {
	x_cols <- select(x, l_v, r_v)
	is_any_char <- any(map_chr(x_cols, class)=="character")
	if (is_any_char) {
		x <- mutate(
			x,
			across(
				c(l_v, r_v),
				function(x) {
					out <- ifelse(!(x %in% c("0", "1")), "0", "1")
					return(out)
				}
			)
		)
	}
	return(x)
}

# stats utils ------------------------------------------------------------------

# adjusted proportion from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)

# adjusted proportion SE from Gelman, Hill & Vehtari (2020)
prop_adj_se <- function(y, n) {
	prop <- prop_adj(y, n)
	sqrt(prop*(1-prop)/(n+4))
}

# adjusted proportion CI from Gelman, Hill & Vehtari (2020)
prop_adj_ci <- function(y, n, conf = 0.95) {
	prop <- (y+2)/(n+4)
	se <- sqrt(prop*(1-prop)/(n+4))
	ci <-  prop + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
	ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
	ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
	return(ci)
}




