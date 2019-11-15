# osf_download_node: Download OSF node

osf_download_component <- function(
  
  project,
  component,
  local.path = getwd()
  
){
  
  # load packages
  require(rlang, quietly = TRUE, warn.conflicts = FALSE)
  require(osfr, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  
  # update local path
  updated.path <- paste0({{ local.path }}, "/", {{ component }})
  # get node
  folders <- osf_retrieve_node({{ project }}) %>%
    osf_ls_nodes() %>%
    filter(name == {{ component }}) %>%
    osf_ls_files() %>%
    pull(name)
  zip.path <- paste0(updated.path, "/", folders, ".zip")
  folder.path <- paste0({{ updated.path }}, "/", folders)
  
  message("OSF component accessed") # show progress
  
  # download
  for (i in 1:length(folders)){
    
    osf_retrieve_node({{ project }}) %>%
      osf_ls_nodes() %>%
      filter(name == {{ component }}) %>%
      osf_ls_files() %>%
      filter(name == folders[i]) %>%
      osf_download(path = folder.path[i], overwrite = TRUE)
    
    # replace current folder
    if(dir.exists(file.path(folder.path[i]))){
      unlink(file.path(folder.path[i]), recursive = TRUE)
    }
    dir.create(file.path(folder.path[i]))
    
    # unzip
    unzip(                                                    
      zipfile = zip.path[i],
      overwrite = TRUE,
      exdir = folder.path[i]
    )
    
    # remove ZIP
    file.remove(zip.path[i])        
    
    # show progress
    message(paste0(folders[i], " downloaded")) # show progress
  }
  
}
