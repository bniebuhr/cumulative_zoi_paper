#' Find layers within GRASS GIS with multiple patterns
#' 
#' @param list_patterns `[list,character]` \cr 
#' 
#' @export
find_layer_GRASS <- function(list_patterns, layers_grass = NULL, type = "raster", pattern = "*", mapset = NULL) {
  
  # get all raster layers from GRASS, if this is not an input
  if(is.null(layers_grass))
    layers_grass <- rgrass7::execGRASS("g.list", type = type, 
                                       pattern = pattern, 
                                       mapset = mapset) %>% 
      attr(., "resOut")
  
  # apply all filters according to the patterns
  layers_grass_filt <- layers_grass 
  for(i in 1:length(list_patterns)) {
    patt <- list_patterns[[i]]
    layers_grass_filt <- layers_grass_filt %>% 
      grep(pattern = patt, value = T)
  }
  
  # return filtered layer
  layers_grass_filt
}

# find_layer_GRASS <- function(infrastructure, measure, shape, zoi, layers_grass = NULL, type = "raster", pattern = "*", mapset = NULL) {
#   
#   # get all raster layers from GRASS, if this is not an input
#   if(is.null(layers_grass))
#     layers_grass <- rgrass7::execGRASS("g.list", type = type, 
#                                        pattern = pattern, 
#                                        mapset = mapset) %>% 
#       attr(., "resOut")
#   
#   # set
#   layers_grass <- layers_grass %>% 
#     grep(pattern = infrastructure, value = T) %>% 
#     grep(pattern = measure, value = T) %>% 
#     grep(pattern = shape, value = T) %>% 
#     grep(pattern = zoi, value = T)
#   
#   layers_grass
# }
