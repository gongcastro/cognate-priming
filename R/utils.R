# run the targets pipeline using clustermq
make <- function() {
	
	job::job(
		{{ 
			targets::tar_make_clustermq(workers = 3)
			job::export("none")  # return nothing
		}}, 
		import = NULL,
		title = "Cognate Priming"
		
	)
}

# load all built targets (and packages)
tar_load_all <- function(){
	
	invisible({
		
		suppressMessages({ tar_load_globals() })
		
		tars <- tar_objects()
		lapply(tars, tar_load_raw, envir = .GlobalEnv)
		
		usethis::ui_done("Targets loaded: ")
		usethis::ui_line(tars)
		
	})
}

# destroy targets products
unmake <- function(keep_rds = TRUE) {
	tar_destroy(ask = FALSE)
	
	rds_paths <- list.files("results/", pattern = ".rds", full.names = TRUE)
	
	if (!keep_rds){
		
		if (length(rds_paths) > 0) {
			lapply(rds_paths, file.remove)
		}	
		
	}
	
	usethis::ui_done("Removed project outputs!")
}

# custom ggplot theme
theme_custom <- function(){
	theme(
		panel.background = element_rect(fill = "white", colour = NA),
		panel.grid = element_blank(),
		plot.background = element_rect(fill = "white", color = NA),
		panel.border = element_blank(),
		axis.line = element_line(colour = "black", size = 0.75),
		legend.key = element_rect(fill = "white", colour = NA),
		text = element_text(colour = "black", size = 15),
		axis.text.x = element_text(size = 12, face = "bold"),
		axis.text.y = element_text(size = 12, face = "bold"),
		strip.text = element_text(size = 13, face = "bold"),
		strip.background = element_rect(fill = "white", colour = NA)
		
	)
}

# dark GitHub theme
theme_github <- function(){
	theme_dark() +
		theme(
			text = element_text(colour = "white", size = 12),
			axis.text = element_text(colour = "white", size = 12, face = "bold"),
			legend.title = element_text(colour = "white", size = 10),
			legend.text = element_text(colour = "white", size = 10),
			legend.background = element_rect(fill = "#0D1117"),
			strip.text = element_text(size = 13),
			legend.key = element_rect(fill = "#0D1117", colour = "#0D1117"),
			strip.background = element_rect(fill = "#161B22"),
			plot.background = element_rect(fill = "#0D1117"),
			panel.background = element_rect(fill = "#0D1117"),
			panel.border = element_rect(fill = "transparent", colour = "#0D1117")
		)
}

# get multilex data ----
get_credentials <- function(){
	ml_connect()
}

get_multilex <- function(
		update = FALSE,
		type = "understands"
){
	get_credentials()
	
	p <- ml_participants()
	r <- ml_responses(p, update = update)
	l <- ml_logs(p, r) %>% 
		rename(participant = id_exp) %>% 
		mutate(
			age_group =  as.factor(
				case_when(
					between(age, 19, 24) ~ 21,
					between(age, 24, 28) ~ 25,
					between(age, 28, 34) ~ 30
				)),
			age_group = paste0(age_group, " months")
		) %>% 
		filter(age_group != "NA months") %>% 
		select(participant, time, age_group, lp)
	
	v <- ml_vocabulary(p, r, scale = "prop", by = "id_exp") %>% 
		filter(type==type)
	
	m <- list(
		participants = p, responses = r, logs = l,
		vocabulary = v, pool = multilex::pool
	)
	
	return(m)
}

# adjusted proportion, SE, and CI ----
# from Gelman, Hill & Vehtari (2020)
prop_adj <- function(y, n) (y+2)/(n+4)

prop_adj_se <- function(y, n) {
	prop <- prop_adj(y, n)
	sqrt(prop*(1-prop)/(n+4))
}

prop_adj_ci <- function(y, n, conf = 0.95) {
	prop <- (y+2)/(n+4)
	se <- sqrt(prop*(1-prop)/(n+4))
	ci <-  prop + qnorm(c((1-.width)/2, (1-(1-.width)/2)))*se
	ci[1] <- ifelse(ci[1]<0, 0, ci[1]) # truncate at 0
	ci[2] <- ifelse(ci[2]>1, 1, ci[2]) # truncate at 1
	return(ci)
}

# inverse logi
inv_logit <- function(x) exp(x)/(1+exp(x))

# transform logit scale to probability
logit_to_prob <- function(x) exp(x) / (1 + exp(x))




