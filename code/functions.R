library(mobsim)
library(raster)

# function to simulate points in the landscape
set_points <- function(n_features = 1000, centers = 1,
                       width = 0.05, res = 0.1,
                       extent_x = c(0,1), extent_y = c(0,1),
                       buffer_around = 0) {
  
  # simulate points
  pts <- sim_thomas_community(s_pool = 1, n_sim = n_features, 
                              sigma = width, mother_points = centers,
                              xrange = extent_x, yrange = extent_y)$census[,1:2]
  
  # raster
  buff <- buffer_around
  r <- raster(xmn = extent_x[1]-buff, xmx = extent_x[2]+buff, 
              ymn = extent_y[1]-buff, ymx = extent_y[2]+buff, 
              resolution = res)
  # resterize points
  r_pts <- rasterize(pts, r, field = 1)
  
  # retun
  list(pts = pts, rast = r_pts)
}

# use
ext <- 30000
wd <- ext/5
pts <- set_points(n_features = 1000, centers = 1,
                  width = wd, res = 100,
                  extent_x = c(0, ext), extent_y = c(0, ext),
                  buffer_around = 10000)
plot(pts$pts)
plot(pts$rast)

wd <- ext/20
pts <- set_points(n_features = 1000, centers = 1,
                  width = wd, res = 100,
                  extent_x = c(0, ext), extent_y = c(0, ext),
                  buffer_around = 10000)
plot(pts$pts)
plot(pts$rast)

# function to calculate dist and density
calc_dist_dens <- function(points, 
                           type_density = c("Gauss", "circle", "rectangle")[1],
                           scale = 100,
                           extent_x_cut = c(0,1), extent_y_cut = c(0,1),
                           plotit = FALSE) {
  
  # distance
  dist_r <- distance(points$rast)
  if(plotit) plot(dist_r)
  
  # density
  r0 <- points$rast
  r0[is.na(r0[])] <- 0 # binary map
  # plot(r0)
  # Gaussian weight
  if(length(scale) == 1) {
    gf <- focalWeight(r0, d = scale, type = type_density)
    density_r <- focal(r0, w = gf)
  } else {
    dens <- sapply(scale, function(x) {
      gf <- focalWeight(r0, d = x, type = type_density)
      focal(r0, w = gf)
      })
    density_r <- stack(dens)
  }
  if(plotit) plot(density_r)
  names_density <- paste0("density", scale)
  
  r_stk <- stack(dist_r, density_r)
  names(r_stk) <- c("distance", names_density)
  crop(r_stk, extent(c(extent_y_cut, extent_x_cut)))
}

# use

# 1 
ext <- 30000
wd <- ext/5
pts <- set_points(n_features = 1000, centers = 1,
                  width = wd, res = 100,
                  extent_x = c(0, ext), extent_y = c(0, ext),
                  buffer_around = 10000)
plot(pts$pts)
plot(pts$rast)

# 2
scales <- c(250, 500, 1000, 2500, 5000)/2
dist_dens <- calc_dist_dens(pts, type_density = "Gauss", scale = scales, 
                            extent_x_cut = c(0, 10000), extent_y_cut = c(0, 10000))
plot(dist_dens)

# function to simulate points and calc distance and density rasters
simulate_rasters <- function(n_features = 1000, centers = 1,
                             width = 0.05, res = 0.1,
                             extent_x = c(0,1), extent_y = c(0,1),
                             buffer_around = 0,
                             type_density = c("Gauss", "circle", "rectangle")[1],
                             scale = 100,
                             extent_x_cut = c(0,1), extent_y_cut = c(0,1)) {
  
  pts <- set_points(n_features = n_features, centers = centers,
                    width = width, res = res,
                    extent_x = extent_x, extent_y = extent_y,
                    buffer_around = buffer_around)
  
  calc_dist_dens(pts, type_density = type_density, scale = scale, 
                 extent_x_cut = extent_x_cut, extent_y_cut = extent_y_cut)
}

# use

# random
ext <- 30000
wd <- ext
scales <- c(250, 500, 1000, 2500, 5000)/2
rasts <- simulate_rasters(n_features = 1000, centers = 1,
                          width = wd, res = 100,
                          extent_x = c(0, ext), extent_y = c(0, ext), 
                          buffer_around = 10000,
                          type_density = "Gauss", scale = scales,
                          extent_x_cut = c(0, ext), extent_y_cut = c(0, ext))
plot(rasts)

# gradient
wd <- ext/3
scales <- c(250, 500, 1000, 2500, 5000)/2
rasts <- simulate_rasters(n_features = 1000, centers = 1,
                          width = wd, res = 100,
                          extent_x = c(0, ext), extent_y = c(0, ext), 
                          buffer_around = 10000,
                          type_density = "Gauss", scale = scales,
                          extent_x_cut = c(0, ext), extent_y_cut = c(0, ext))
plot(rasts)

# single
wd <- ext/20
scales <- c(250, 500, 1000, 2500, 5000)/2
rasts <- simulate_rasters(n_features = 1000, centers = 1,
                          width = wd, res = 100,
                          extent_x = c(0, ext), extent_y = c(0, ext), 
                          buffer_around = 10000,
                          type_density = "Gauss", scale = scales,
                          extent_x_cut = c(0, ext), extent_y_cut = c(0, ext))
plot(rasts)
####### check it to stay within [0, 10km] - change approach

# multiple
wd <- 0.1*ext
scales <- c(250, 500, 1000, 2500, 5000)/2
rasts <- simulate_rasters(n_features = 1000, centers = 5,
                          width = wd, res = 100,
                          extent_x = c(0, ext), extent_y = c(0, ext), 
                          buffer_around = 10000,
                          type_density = "Gauss", scale = scales,
                          extent_x_cut = c(0, ext), extent_y_cut = c(0, ext))
plot(rasts)
