# run the targets pipeline
make <- function() {
	job::job({{ 
		targets::tar_make()
		job::export("none")  # return nothing
	}}, 
	import = NULL,
	title = "Cognate Priming")
}

# destroy targets products
unmake <- function(keep_rds = TRUE) {
	tar_destroy(ask = FALSE)
	rds_paths <- list.files("results/", pattern = ".rds", full.names = TRUE)
	if (!keep_rds){
		if (length(rds_paths) > 0) lapply(rds_paths, file.remove)
	}
	cli::cli_alert_success("Removed project outputs!")
}

# custom ggplot theme
theme_custom <- function(){
	theme(panel.background = element_rect(fill = "white", colour = NA),
		  panel.border = element_blank(),
		  panel.grid = element_blank(),
		  plot.background = element_rect(fill = "white", color = NA),
		  legend.key = element_rect(fill = "white", colour = NA),
		  text = element_text(colour = "black", size = 15),
		  axis.text.x = element_text(size = 12, face = "bold"),
		  axis.text.y = element_text(size = 12, face = "bold"),
		  axis.line = element_line(colour = "black", size = 0.75),
		  strip.text = element_text(size = 13, face = "bold"),
		  strip.background = element_rect(fill = "white", colour = NA))
}

# taken from https://www.anthonyschmidt.co/post/2020-06-03-making-apa-tables-with-gt/
gt_apa <- function(x, title = " "){
	x %>% 
		tab_options(
			table.border.top.color = "white",
			table_body.hlines.width = 0.75,
			heading.title.font.size = px(16),
			column_labels.border.top.width = 0.75,
			column_labels.border.top.color = "black",
			column_labels.border.bottom.width = 0.75,
			column_labels.border.bottom.color = "black",
			column_labels.vlines.width = 0,
			stub.border.color = "black",
			row_group.border.top.width = 0,
			row_group.border.bottom.width = 0,
			summary_row.border.color = "black",
			summary_row.border.width = 0.75,
			grand_summary_row.border.color = "black",
			grand_summary_row.border.width = 0.75,
			stub_row_group.border.width = 0,
			stub.border.width = 0,
			table_body.border.bottom.color = "black",
			table.border.bottom.color = "white",
			table.width = pct(100),
			table.background.color = "white",
		) %>%
		cols_align(align="center") %>%
		tab_style(
			style = list(
				cell_borders(
					sides = c("top", "bottom"),
					color = "white",
					weight = px(1)
				),
				cell_text(
					align="center"
				),
				cell_fill(color = "white", alpha = NULL)
			),
			locations = cells_body(
				columns = everything(),
				rows = everything()
			)
		) %>%
		#title setup
		tab_header(
			title = html("<i>", title, "</i>")
		) %>%
		opt_align_table_header(align = "left")
}

# get name dictionary 
get_name_dictionary <- function(...) {
	
	col_name_changes <- c(
		"participant" = "id",
		"system_time_stamp" = "time",
		"l_1" = "l_x",
		"l_2" = "l_y",
		"process" = "phase",
		"r_1" = "r_x",
		"r_2" = "r_y",
		"l_user_coord_3" = "l_user_coord_z",
		"r_user_coord_3" = "r_user_coord_z",
		"suje_num" = "id"
	)
	
	phase_name_changes <- c(
		"GETTER" = "Getter",
		"PRIMEIMAGE" = "Prime",
		"TARGET_DISTRACTOR" = "Target-Distractor",
		"prime" = "Prime",
		"primeimage" = "Prime",
		"target_distractor" = "Target-Distractor"
	)
	
	relevant_variables <- c("id", "trial_num", "trial", "phase", "time", "l_x", "l_y", "l_v", "r_x", "r_y", "r_v")
	
	name_dict <- lst(col_name_changes, phase_name_changes, relevant_variables)
	
	return(name_dict)
}

# get BVQ data
get_bvq <- function(update = FALSE,
					type = "understands"
){
	p <- bvq_participants()
	r <- bvq_responses(p, update = update)
	l <- bvq_logs(p, r) %>% 
		select(-id) %>% 
		rename(id = id_exp) %>% 
		mutate(age_group = as.factor(case_when(between(age, 19, 24) ~ "21 months",
											   between(age, 24, 28) ~ "25 months",
											   between(age, 28, 34) ~ "30 months",
											   TRUE ~ "Other"))) %>% 
	select(id, time, age_group, lp)
	
	v <- bvq_vocabulary(p, r, scale = "prop", by = "id_exp") %>% 
		select(-id) %>% 
		rename(id = id_exp) %>% 
		filter(type==.env$type) %>% 
		mutate(age_group =  as.factor(case_when(between(age, 19, 24) ~ "21 months",
												between(age, 24, 28) ~ "25 months",
												between(age, 28, 34) ~ "30 months",
												TRUE ~ "Other"))) %>% 
		left_join(distinct(l, id, age_group)) %>% 
		relocate(id, time, age_group, age, type)
	
	bvq <- list(participants = p, 
				responses = r, 
				logs = l, 
				vocabulary = v, 
				pool = bvqdev::pool)
	
	return(bvq)
}

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




