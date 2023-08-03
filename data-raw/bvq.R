# get participants
p <- bvq::bvq_participants()

# get responses
r <- bvq::bvq_responses(p)

# get logs
edu_dict <- c("noeducation" = "No education",
			  "primary" = "Primary",
			  "secondary" = "Secondary",
			  "complementary" = "Complementary",
			  "vocational" = "Vocational",
			  "university" = "University")

l <- bvq::bvq_logs(p, r) |> 
	mutate(age_group = case_when(age>=19 & age<24 ~ "21 months",
								 age>=24 & age<28 ~ "25 months",
								 age>=28 & age<=34 ~ "30 months",
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
	left_join(select(p, id, time, id_exp),
			  by = join_by(id, time)) |> 
	distinct(id, age_group, .keep_all = TRUE) |> 
	select(id, id_exp, time, age, age_group, lp, 
		   dominance, doe_catalan, doe_spanish, edu_parent)

v_comp <- bvq::bvq_vocabulary(p, r) |> 
	dplyr::filter(type=="understands") |> 
	inner_join(l, by = join_by(id, time)) |> 
	select(id, id_exp, age_group, type, matches("prop|count"))

v_prod <- bvq::bvq_vocabulary(participants = p, 
							  responses = r, 
							  .scale = c("prop", "count")) |> 
	dplyr::filter(type=="produces") |> 
	inner_join(distinct(l, id, time, id_exp, age_group),
			   by = join_by(id, time)) |> 
	select(id, id_exp, age_group, type, matches("prop|count"))

bvq_data <- list(participants = p, 
				 responses = r, 
				 logs = l, 
				 vocabulary_comp = v_comp,
				 vocabulary_prod = v_prod,
				 pool = bvq::pool)

saveRDS(bvq_data, "data-raw/bvq.rds")
