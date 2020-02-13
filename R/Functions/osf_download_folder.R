# osf_download_node: Download OSF node

osf_download_folder <- function(
	
	PAT,
	project,
	component,
	folder,
	local.path
	
){
	
	# load packages
	require(rlang, quietly = TRUE, warn.conflicts = FALSE)
	require(osfr, quietly = TRUE, warn.conflicts = FALSE)
	require(magrittr, quietly = TRUE, warn.conflicts = FALSE)
	
	# personal access token
	osf_auth({{ PAT }})
	
	# get node
	folder <- osf_retrieve_node({{ project }}) %>%
		osf_ls_nodes(pattern = {{ component }}) %>%
		osf_ls_files(pattern = {{ folder }}) %$%
		name
	
	zip.path <- paste0({{ local.path }}, ".zip")

	message("OSF component accessed") # show progress
	
	# download
	osf_retrieve_node({{ project }}) %>%
		osf_ls_nodes({{ component }}) %>%
		osf_ls_files(pattern = {{ folder }}) %>%
		osf_download(path = local.path, overwrite = TRUE)
	
	# replace current folder
	if(dir.exists(file.path(local.path))){
		unlink(file.path(local.path), recursive = TRUE)
	}
	dir.create(file.path(local.path))
	
	# unzip
	unzip(                                                    
		zipfile   = zip.path,
		overwrite = TRUE,
		exdir     = local.path
	)
	
	# remove ZIP
	file.remove(zip.path)        
	
	# show progress
	message(paste0("OSF download finished")) # show progress
}

