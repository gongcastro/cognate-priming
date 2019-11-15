# image_get_size: Measure image size when displayed in screen
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

##########################################################

image_get_size <- function(
  
  input.path    = getwd(), # folder
  distance      = 60,      # distance from screen to eyes (in cm)
  screen.width  = 1920,    # screen width (in pixels)
  screen.height = 1080,    # screen height (in pixels)
  dpi           = 96,      # dots per inch (screen resolution)
  units         = 'cm'     # units to be used (cm or in)     
){
  
  # set up
  require(magick) # for image manipulation
  require(dplyr)  # for data manipulation
  require(purrr)  # for functional programming
  
  # transform pixels to mm
  pixel.size <- ifelse(units == 'cm', 2.54/dpi, dpi) # calculate pixel size in cm
  
  # calculate size in degrees
  list.files(input.path, pattern = '.png|.jpg', full.names = TRUE) %>% # list files in folder
    map(~image_read(.)) %>%                                            # import images
    set_names(., list.files(input.path, pattern = '.png|.jpg')) %>%    # name each image with filename
    map(~image_info(.)) %>%                                            # get image information
    map(., `[`, c('width', 'height')) %>%                              # extract image width and height (in pixels)
    bind_rows(., .id = 'names') %>%                                    # merge information from all images into a table
    mutate(
      width = width*pixel.size,                        # width in real units (cm or inches)
      height = height*pixel.size,                      # height in real units (cm or inches)
      angle.vertical = (1/tan((width/2)/distance)),    # vertical angle from eye
      angle.horizontal = (1/tan((height/2)/distance)), # horizontal angle from eye
      surface = width*height*pi
    ) 
    
}

