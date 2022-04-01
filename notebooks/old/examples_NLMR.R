#-----------
# representation of ecotones
library(NLMR)
library(landscapetools)

# landscape with higher autocorrelation 
high_autocorrelation <- nlm_edgegradient(ncol = 100, nrow = 100, direction = 80)

# landscape with lower autocorrelation 
low_autocorrelation <- nlm_fbm(ncol = 100, nrow = 100, fract_dim = 0.5)

# merge to derive ecotone
ecotones <- util_merge(low_autocorrelation, high_autocorrelation)

# look at the results
show_landscape(list("Low autocorrelation" = low_autocorrelation,
                    "High autocorrelation" = high_autocorrelation,
                    "Ecotones" = ecotones
))

#------------
# multiple inputs as a grid
library(NLMR)
library(landscapetools)
library(dplyr)
library(purrr)
library(tibble)

# simulation function that has the parameters we want to alter as input
simulate_landscape = function(roughness, weighting){
  nlm_mpd(ncol = 33,
          nrow = 33,
          roughness = roughness,
          rescale = TRUE) %>%
    util_classify(weighting = weighting)
}

# paramter combinations we are interested in
param_df = expand.grid(roughness = c(0.2, 0.9),
                       weighting = list(c(0.2, 0.8), c(0.2, 0.3, 0.5))) %>%
  as_tibble()

# map over the nested tibble and use each row as input for our simulation function
nlm_list = param_df %>% pmap(simulate_landscape)

# look at the results
show_landscape(nlm_list)

#------------------------
# simulate landscapes

# example gaussian field
NLMR::nlm_gaussianfield(100, 100, 100, autocorr_range = 100, mag_var = 10, nug = 0.001, user_seed = 55) %>% 
    landscapetools::show_landscape()
# the main argument that changes clumpiness seems to be 

# example fractional Brownian motion
NLMR::nlm_fbm(100, 100, 100, fract_dim = 0.1, user_seed = 123) %>% 
  landscapetools::show_landscape()

# example of random cluster
NLMR::nlm_randomcluster(nrow = 100, ncol = 100, p = 0.5, 
                        ai = c(1, 0.6, 0.1), rescale = FALSE) %>% 
  landscapetools::show_landscape()

# example of midpoint displacement
set.seed(123)
NLMR::nlm_mpd(100, 100, resolution = 100, roughness = 0.3, rand_dev = 1,
              rescale = TRUE, verbose = TRUE) %>% 
  landscapetools::show_landscape()

#------------------
# understand gaussian field parameters

# parameters
param_df <- expand.grid(ncol = 100,
                        nrow = 100,
                        resolution = 100, 
                        autocorr_range = c(1, 5, 10, 100, 500, 1000)) %>% 
  tibble::as_tibble()

# param_df <- expand.grid(ncol = 100,
#                         nrow = 100,
#                         resolution = 100, 
#                         autocorr_range = 100,
#                         nug = c(0.001, 0.01, 0.05, 1, 2, 5)) %>% 
#   tibble::as_tibble()

# simulated landscapes
nlm_list = param_df %>% pmap(nlm_gaussianfield, user_seed = 123)

# plot
landscapetools::show_landscape(nlm_list)

#------------------
# understand fractional Brownian motion parameters

# parameters
param_df <- expand.grid(ncol = 100,
                        nrow = 100,
                        resolution = 100, 
                        fract_dim = c(0.001, 0.01, 0.05, 0.08, 1, 1.2, 1.4, 1.6, 1.8, 2)) %>% 
  tibble::as_tibble()

# subset
# param_df <- expand.grid(ncol = 100,
#                         nrow = 100,
#                         resolution = 100, 
#                         fract_dim = c(0.001, 0.1, 0.5, 1, 1.6)) %>% 
#   tibble::as_tibble()

# simulated landscapes
nlm_list = param_df %>% pmap(nlm_fbm, user_seed = 123)

# plot
landscapetools::show_landscape(nlm_list)

#------------------
# understand midpoint displacement parameters

# parameters
param_df <- expand.grid(ncol = 100,
                        nrow = 100,
                        resolution = 100, 
                        roughness = c(seq(0.1, 1, 0.1), 2), #0.5,
                        rand_dev = 1 #c(0.1, 1, 5, 10, 100)
                        ) %>% 
  tibble::as_tibble()

# subset
param_df <- expand.grid(ncol = 100,
                        nrow = 100,
                        resolution = 100, 
                        roughness = c(0.1, 0.3, 0.5, 0.7, 0.9, 2), #0.5,
                        rand_dev = 1 #c(0.1, 1, 5, 10, 100)
                        ) %>% 
  tibble::as_tibble()

# simulated landscapes
nlm_mpd_seed <- function(user_seed = 123, ...) {
  set.seed(user_seed)
  NLMR::nlm_mpd(...)
}

nlm_list <- param_df %>% pmap(nlm_mpd_seed, user_seed = 123)
names(nlm_list) <- param_df$roughness

# plot
landscapetools::show_landscape(nlm_list)

#-------------------------------
# parenthesis to understand
# runif with vectors as parameters

runif(3, 0, 1)
runif(3, c(0, 0, -1), c(1, 5, 0))
runif(6, c(0, 0, -1), c(1, 5, 0))
      
a <- replicate(100, runif(3, c(0, 0, -1), c(1, 5, 0)))
hist(a[1,])
hist(a[2,])
hist(a[3,])

#-------------------------------
# simulate points

# example adapted from https://gis.stackexchange.com/questions/224321/randomly-generate-points-using-weights-from-raster

# example gaussian field
nlm1 <- NLMR::nlm_gaussianfield(100, 100, 100, autocorr_range = 100, mag_var = 10, nug = 0.001, user_seed = 123)
landscapetools::show_landscape(nlm1)

n_points <- 1000
res <- 100
r <- nlm1
ptscell <- sample(1:length(r), n_points, prob = r[], replace=TRUE)
center <- raster::xyFromCell(r, ptscell)
pts <- cbind(runif(nrow(center), center[,1] - res/2, center[,1] + res/2), 
             runif(nrow(center), center[,2] - res/2, center[,2] + res/2))

raster::plot(r)
points(pts)
