library(bvq)
library(dplyr)

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
	mutate(age_group = case_when(age >= 19 & age < 24 ~ "21 months",
								 age >= 24 & age < 28 ~ "25 months",
								 age >= 28 & age <= 34 ~ "30 months",
								 TRUE ~ "Other"),
		   age_group = as.factor(age_group),
		   across(starts_with("edu_"), 
		   	   function(x) {
		   	   	factor(x, 
		   	   		   levels = names(edu_dict),
		   	   		   ordered = TRUE) |> 
		   	   		as.numeric()
		   	   }),
		   # get maximum educational attainment of parents
		   edu_parent = apply(cbind(edu_parent1, edu_parent2), 1,
		   				   function(x) max(x, na.rm = FALSE)),
		   # recode it as factor
		   edu_parent = factor(edu_parent,
		   					levels = 1:6, 
		   					labels = edu_dict)) |>
	left_join(select(p, child_id, response_id, time),
			  by = join_by(child_id, response_id, time)) |> 
	distinct(child_id, response_id, time, age_group, .keep_all = TRUE) |> 
	select(child_id, response_id, time, age, age_group, lp, version,  
		   dominance, doe_catalan, doe_spanish, doe_others, edu_parent)

v <- bvq_vocabulary(p, r, .scale = c("count", "prop")) |> 
	dplyr::filter(type=="understands") |> 
	inner_join(l, by = join_by(child_id, response_id)) |> 
	filter(type=="understands") |> 
	inner_join(l, by = join_by(child_id, response_id, time)) |> 
	select(child_id, response_id, time, matches("prop|count"), contents) 


bvq_data <- list(participants = p, 
				 responses = r, 
				 logs = l, 
				 vocabulary = v,
				 pool = pool)

saveRDS(bvq_data, file.path("data-raw", "bvq", "bvq.rds"))
