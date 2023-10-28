
library(bvq)

# get participants
p <- bvq_participants()

# get responses
r <- bvq_responses(p)

# get logs
edu_dict <- c("noeducation" = "No education",
			  "primary" = "Primary",
			  "secondary" = "Secondary",
			  "complementary" = "Complementary",
			  "vocational" = "Vocational",
			  "university" = "University")

l <- bvq_logs(p, r) |> 
	# get maximum educational attainment of parents
	mutate(edu_parent = apply(cbind(edu_parent1, edu_parent2), 1,
							  function(x) max(x, na.rm = FALSE)) |>
		   	factor(levels = names(edu_dict), 
		   		   labels = edu_dict)) |>
	left_join(select(p, child_id, response_id, time),
			  by = join_by(child_id, response_id, time)) |> 
	distinct(response_id, .keep_all = TRUE) |> 
	select(child_id, response_id, time, age, lp, version, dominance, edu_parent)

v <- bvq_vocabulary(p, r, .scale = c("count", "prop")) |> 
	dplyr::filter(type=="understands") |> 
	inner_join(l, by = join_by(child_id, response_id)) |> 
	filter(type=="understands") |> 
	select(child_id, response_id, time, matches("prop"), contents) |> 
	inner_join(l, by = join_by(child_id, response_id, time)) |> 
	select(response_id, matches("prop|count"), contents ) 


bvq_data <- list(participants = p, 
				 responses = r, 
				 logs = l, 
				 vocabulary = v,
				 pool = pool)

saveRDS(bvq_data, file.path("data-raw", "bvq.rds"))
