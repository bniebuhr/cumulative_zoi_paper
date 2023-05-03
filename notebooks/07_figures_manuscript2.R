# packages
library(terra)
library(sf)
library(tmap)
library(oneimpact)
# install.packages("layer", dep = T)
# library(devtools)
# devtools::install_github("bniebuhr/layer")
library(layer)
library(ggplot2)

#-------
# data - vector
(f <- system.file("vector/sample_area_roads.gpkg", package = "oneimpact"))
v <- sf::st_read(f)

# data - raster
(f2 <- system.file("raster/sample_area_roads.tif", package = "oneimpact"))
r <- terra::rast(f2)
names(r) <- "roads"

#-------
# basic plots
p_v <- tmap::tm_shape(v) +
  tm_lines()

p_r <- tmap::tm_shape(r) +
  tm_raster(palette = "black", legend.show = F)

# export
tmap::tmap_save(p_v, filename = "text/figures/f2_roads_vect.png", 
                width = 5, height = 3.8, units = "cm", dpi = 300)
tmap::tmap_save(p_r, filename = "text/figures/f2_roads_rast.png", 
                width = 5, height = 3.8, units = "cm", dpi = 300)

#----
# zoi

#---
# nearest
rad <- c(1000, 3000, 5000)
zoi_n <- calc_zoi(r, radius = rad, type = "Gauss",
                  zoi_metric = "nearest", zeroAsNA = TRUE)
zoi_n
plot(zoi_n)

# plot
tilt_landscape_1 <- tilt_map(zoi_n[[1]],
                             x_stretch = 1, y_stretch = 0,
                             x_tilt = 0, y_tilt = 1,
                             angle_rotate = 0)
tilt_landscape_2 <- tilt_map(zoi_n[[2]],
                             x_stretch = 1, y_stretch = 0,
                             x_tilt = 0, y_tilt = 1,
                             x_shift = 3000, y_shift = -2000,
                             angle_rotate = 0)
tilt_landscape_3 <- tilt_map(zoi_n[[3]],
                             x_stretch = 1, y_stretch = 0,
                             x_tilt = 0, y_tilt = 1,
                             x_shift = 6000, y_shift = -4000,
                             angle_rotate = 0)
# ggplot() + geom_sf(data = tilt_landscape_1, colour = NA, aes(fill = value))
p_near <- plot_tiltedmaps(list(tilt_landscape_1, tilt_landscape_2, tilt_landscape_3),
                          layer = c("value", "value", "value"),
                          palette = c("viridis", "viridis", "viridis"))

# export
ggplot2::ggsave(p_near, filename = "f2_roads_nearest.png", path = "text/figures/", 
                width = 5*1.2, height = 3.8*1.2, units = "cm", dpi = 300)

#---
# cumulative
rad <- c(1000, 3000, 5000)
zoi_c <- calc_zoi(r, radius = rad, type = "Gauss",
                  zoi_metric = "cumulative", zeroAsNA = FALSE)
zoi_c
plot(zoi_c)

# plot
tilt_landscape_c1 <- tilt_map(zoi_c[[1]],
                              x_stretch = 1, y_stretch = 0,
                              x_tilt = 0, y_tilt = 1,
                              angle_rotate = 0)
tilt_landscape_c2 <- tilt_map(zoi_c[[2]],
                              x_stretch = 1, y_stretch = 0,
                              x_tilt = 0, y_tilt = 1,
                              x_shift = 3000, y_shift = -2000,
                              angle_rotate = 0)
tilt_landscape_c3 <- tilt_map(zoi_c[[3]],
                              x_stretch = 1, y_stretch = 0,
                              x_tilt = 0, y_tilt = 1,
                              x_shift = 6000, y_shift = -4000,
                              angle_rotate = 0)
# ggplot() + geom_sf(data = tilt_landscape_1, colour = NA, aes(fill = value))
p_cum <- plot_tiltedmaps(list(tilt_landscape_c1, tilt_landscape_c2, tilt_landscape_c3),
                         layer = c("value", "value", "value"),
                         palette = c("viridis", "viridis", "viridis"))

# export
ggplot2::ggsave(p_cum, filename = "f2_roads_cumulative.png", path = "text/figures/", 
                width = 5*1.2, height = 3.8*1.2, units = "cm", dpi = 300)

#----------
# other maps
library(NLMR)

# 1.
edge_nlm <- nlm_edgegradient(478, 361)
distance_nlm <- nlm_distancegradient(100, 100, origin = c(20, 20,10, 10))

# 2.
gauss_nlm <- nlm_gaussianfield(478, 361)
rectan_nlm <- nlm_randomrectangularcluster(100, 100, maxl = 30, minl = 10)

# 3.
mosaic_nlm <- nlm_mosaicfield(100, 100)

# 4.
planar_nlm <- nlm_planargradient(100, 100)
tess_nlm <- nlm_mosaictess(478, 361, germs = 200)

# plot it
landscapetools::show_landscape(edge_nlm)
landscapetools::show_landscape(distance_nlm)
landscapetools::show_landscape(gauss_nlm)
landscapetools::show_landscape(rectan_nlm)
landscapetools::show_landscape(mosaic_nlm)
landscapetools::show_landscape(planar_nlm)
landscapetools::show_landscape(tess_nlm)

# plot
tilt_landscape_e1 <- tilt_map(edge_nlm,
                              x_stretch = 1, y_stretch = 0,
                              x_tilt = 0, y_tilt = 1,
                              angle_rotate = 0)
tilt_landscape_e2 <- tilt_map(tess_nlm,
                              x_stretch = 1, y_stretch = 0,
                              x_tilt = 0, y_tilt = 1,
                              x_shift = 30, y_shift = -30,
                              angle_rotate = 0)
tilt_landscape_e3 <- tilt_map(gauss_nlm,
                              x_stretch = 1, y_stretch = 0,
                              x_tilt = 0, y_tilt = 1,
                              x_shift = 60, y_shift = -60,
                              angle_rotate = 0)
# ggplot() + geom_sf(data = tilt_landscape_1, colour = NA, aes(fill = value))
p_env <- plot_tiltedmaps(list(tilt_landscape_e1, tilt_landscape_e2, tilt_landscape_e3),
                         layer = c("value", "value", "value"),
                         palette = c("mako", "cividis", "magma"))

# export
ggplot2::ggsave(p_env, filename = "f2_env_variables.png", path = "text/figures/", 
                width = 5*1.2, height = 3.8*1.2, units = "cm", dpi = 300)
