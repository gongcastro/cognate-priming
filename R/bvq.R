# get BVQ data
get_bvq <- function(type = "understands"){
	
	# get participants
	p <- bvq_participants()
	
	# get responses
	r <- bvq_responses(p)
	
	# get logs
	l <- bvq_logs(p, r) |> 
		mutate(age_group = case_when(age>=19 & age<24 ~ "21 months",
									 age>=24 & age<28 ~ "25 months",
									 age>=28 & age<=34 ~ "30 months",
									 TRUE ~ "Other"),
			   age_group = as.factor(age_group)) |> 
		left_join(select(p, id, time, id_exp),
				  by = join_by(id, time)) |> 
		distinct(id, age_group, .keep_all = TRUE) |> 
		select(id, id_exp, time, age, age_group, lp)
	
	v <- bvq_vocabulary(participants = p, 
						responses = r,
						scale = "prop") |> 
		dplyr::filter(type==.env$type) |> 
		inner_join(distinct(l, id, time, id_exp, age_group),
				   by = join_by(id, time)) |> 
		select(id, id_exp, age_group, type, ends_with("prop"))
	
	bvq_data <- list(participants = p, 
				responses = r, 
				logs = l, 
				vocabulary = v, 
				pool = bvq::pool)
	
	return(bvq_data)
}
