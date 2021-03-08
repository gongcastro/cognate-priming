# utils for stimuli


# extract formants -------------------------------------------------------------
extract_formants <- function(
	file, time_step = 0.001, n = 2, max_freq = 5500, window_length = 0.001, pre = 50,
	include_frames = TRUE, include_time = TRUE, dec = 3, include_intensity = TRUE,
	dec_intensity = 5, include_n_formats = FALSE, dec_freq = 3, include_bandwidths = FALSE
) {
	require(PraatR)
	require(janitor)
	
	file_paths <- list.files(file, pattern = ".wav", full.names = TRUE)
	file_names <- list.files(file, pattern = ".wav", full.names = FALSE)
	
	arguments1 <- list(time_step, n, max_freq, window_length, pre)
	arguments2 <- list(include_frames, include_time, dec, include_intensity, dec_intensity,
					   include_n_formats, dec_freq, include_bandwidths) 
	
	dir <- tempdir()
	dir_formants <- paste(dir, file_names, sep = "/") %>% str_replace(., ".wav", ".Formant")
	dir_formants_table <- paste(dir, file_names, sep = "/")  %>% str_replace(., ".wav", ".txt")
	
	fun1 <- function(x, y) praat("To Formant (burg)...", arguments = arguments1, input = x, output = y, overwrite = TRUE)
	fun2 <- function(x, y) praat("Down to Table...", arguments = arguments2, input = x, output = y, filetype = "tab-separated", overwrite = TRUE)
	
	invisible(map2(.x = as.list(file_paths), .y = as.list(dir_formants), .f = ~fun1(.x, .y)))
	invisible(map2(.x = as.list(dir_formants), .y = as.list(dir_formants_table), .f = ~fun2(.x, .y)))
	
	x <- map(
		dir_formants_table,
		~fread(., header = TRUE, sep = "\t", dec = ".", na.strings = "--undefined--")) %>%
		set_names(str_remove(file_names, ".wav")) %>%
		bind_rows(.id = "file") %>%
		clean_names() %>%
		rename_all(~str_remove(., "_hz")) %>%
		rename_all(~str_remove(., "_s")
		) 
	return(x)
}

# extract pitch
extract_pitch <- function(
	file,
	time_step = 0.001,
	pitch_floor = 50,
	pitch_ceiling = 800
) {
	require(PraatR)
	require(janitor)
	
	file_paths <- list.files({{ file }}, pattern = ".wav", full.names = TRUE)
	file_names <- list.files({{ file }}, pattern = ".wav", full.names = FALSE)
	
	pb <- progress_estimated(length(file_paths)*2)
	arguments <- list({{ time_step }}, {{ pitch_floor }}, {{ pitch_ceiling }})
	
	dir <- tempdir()
	dir1 <- paste(dir, file_names, sep = "/") %>% str_replace(., ".wav", ".Pitch")
	dir2 <- paste(dir, file_names, sep = "/")  %>% str_replace(., ".wav", ".PitchTier")
	
	fun1 <- function(x, y) {
		pb$tick()$print()
		praat("To Pitch...", arguments = arguments, input = x, output = y, overwrite = TRUE)
	}
	
	fun2 <- function(x, y) {
		pb$tick()$print()
		praat("Down to PitchTier", input = x, output = y, overwrite = TRUE, filetype = "headerless spreadsheet")
	}
	
	invisible(map2(.x = as.list(file_paths), .y = as.list(dir1), .f = ~fun1(.x, .y)))
	invisible(map2(.x = as.list(dir1), .y = as.list(dir2), .f = ~fun2(.x, .y)))
	x <- map(
		as.list(dir2),
		~fread(., header = FALSE, sep = "\t", dec = ".", na.strings = "--undefined--")) %>%
		set_names(str_remove(file_names, ".wav")) %>%
		bind_rows(.id = "file") %>%
		clean_names() %>%
		rename(time = v1, f0 = v2)
	return(x)
}

# download clearpond -----------------------------------------------------------
import_clearpond <- function(language = c("english", "dutch", "french", "spanish", "german")){
	require(tidyverse)
	require(janitor)
	urls <- tribble(
		~lang, ~url, ~data, ~header,
		"english", "https://clearpond.northwestern.edu/englishCPdatabase2.zip", "englishCPdatabase2.txt", "clearpondHeaders_EN.txt",
		"dutch", "https://clearpond.northwestern.edu/dutchCPdatabase2.zip", "dutchCPdatabase2.txt", "clearpondHeaders_NL.txt",
		"french", "https://clearpond.northwestern.edu/frenchCPdatabase2.zip", "frenchCPdatabase2.txt", "clearpondHeaders_FR.txt",
		"german", "https://clearpond.northwestern.edu/germanCPdatabase2.zip", "germanCPdatabase2.txt", "clearpondHeaders_DE.txt",
		"spanish", "https://clearpond.northwestern.edu/spanishCPdatabase2.zip", "spanishCPdatabase2.txt", "clearpondHeaders_SP.txt"
	) %>% 
		filter(lang %in% language)
	
	dir <- tempdir()
	files <- replicate(tempfile(), n = length(urls$lang))
	x <- pmap(
		.l = list(url = as.list(urls$url), file = as.list(files), data = as.list(urls$data), header = as.list(urls$header)),
		.f = function(url = .l[[1]], file = .l[[2]], data = .l[[3]], header = .l[[4]]) {
			download.file(url, destfile = file)
			unzip(zipfile = file, exdir = dir)
			headers <- c("word", read.delim(paste0(dir, .Platform$file.sep, header))[,1])
			d <- read.delim(paste0(dir, .Platform$file.sep, data)) %>% 
				`colnames<-`(., headers) %>% 
				as_tibble() %>% 
				mutate_at(vars(ends_with("W")), ~str_split(., pattern = ";"))
			return(d)
		}
	) %>% 
		set_names(language) %>% 
		bind_rows(.id = "language") %>% 
		clean_names()
	return(x)
}
