# utils: helper functions

# evaluate if x is NOT an element of y -----------------------------------------
"%!in%" <- function(x, y){!(x %in% y)}

# replace NaNs with NAs --------------------------------------------------------
nan_to_na <- function(x) ifelse(is.nan(x), NA_real_, x)

# find first/last non-missing value in vector ----------------------------------
first_non_na <- function(x) ifelse(is.logical(first(x[!is.na(x)])), NA, first(x[!is.na(x)]))
last_non_na <- function(x) ifelse(is.logical(last(x[!is.na(x)])), NA, last(x[!is.na(x)]))

# big mark format
add_big_mark <- function(x) format(x, big.mark = ",", scientific = FALSE)

# custom ggplot theme ----------------------------------------------------------
theme_custom <- function(){
	theme(
		panel.background = element_rect(fill = "transparent"),
		panel.grid = element_blank(),
		#panel.grid = element_line(colour = "grey", linetype = "dotted"),
		panel.border = element_rect(fill = "transparent", colour = "black"),
		text = element_text(colour = "black", size = 15),
		axis.text = element_text(colour = "black")
	)
}

