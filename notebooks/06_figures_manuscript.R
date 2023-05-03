# code for figures
library(oneimpact)
library(dplyr)
library(ggplot2)
library(mdthemes)
library(patchwork)
library(png)

house <- jpeg::readJPEG("text/figures/house_drawing.jpg", native = TRUE)

# All curves
zoi <- 3
curves <- c("Threshold", "Linear", "Exponential", "Gaussian")
g1 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  stat_function(fun = exp_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Exponential")) +
  stat_function(fun = threshold_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Threshold")) +
  stat_function(fun = bartlett_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Linear")) +
  stat_function(fun = gaussian_decay, args = list(zoi = zoi, oneside = F, zoi_decay_threshold = 0.05), aes(linetype = "Gaussian")) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.03, yend = 0.03, color = grey(0.3),
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 1.5, y = 0.05, color = grey(0.3), size = 3, label = "Zone of Influence") +
  scale_linetype_manual("Influence  
                        function", 
                        values = 1:4,
                        breaks = curves) +
  labs(x = "Distance, *d* (km)", y = "Influence, \u03C6") +
  # theme_classic(base_size = 14) +
  mdthemes::md_theme_classic(base_size = 14) +
  inset_element(house, left = 0.47, right = 0.53, bottom = 0.01, top = 0.06) 
g1  

# save
ggsave("zoi_shapes_all.png", plot = g1, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

# remove exponential
zoi <- 3
g2 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  # stat_function(fun = exp_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Exponential")) +
  stat_function(fun = threshold_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Threshold")) +
  stat_function(fun = bartlett_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Linear")) +
  stat_function(fun = gaussian_decay, args = list(zoi = zoi, oneside = F, zoi_decay_threshold = 0.05), aes(linetype = "Gaussian")) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.03, yend = 0.03, color = grey(0.3),
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 1.5, y = 0.05, color = grey(0.3), size = 3, label = "Zone of Influence") +
  scale_linetype_manual("Influence  
                        function", 
                        values = 1:4,
                        breaks = curves) +
  labs(x = "Space (km)", y = "Influence, \u03C6") +
  ylim(0, 2) +
  # theme_classic(base_size = 14) +
  mdthemes::md_theme_classic(base_size = 14) +
  inset_element(house, left = 0.47, right = 0.53, bottom = 0.01, top = 0.06) 
g2

# save
ggsave("zoi_shapes_noexp.png", plot = g2, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

#----------------------------------
# settings
points <- c(0.5, 4.5, 7, 8)
zoi <- 2.5

# functions
funs <- c("threshold_decay", "linear_decay", "gaussian_decay")
funs_name <- c("Threshold", "Linear", "Gaussian")
# cumulative
cums <- c(F, T)
cums_name <- c("phi[nearest]", "phi[cumulative]")

df_plots <- list()
i <- 1
for(ff in 1:length(funs)) {
  
  for(cc in 1:length(cums)) {
    
    df_plots[[i]] <- plot_influence2d(points, zoi, cumulative = cums[cc], 
                                      fun = funs[[ff]],
                                      range_plot = c(0, 12), return_df = TRUE)$influence_df %>% 
      dplyr::mutate(fun = funs_name[[ff]], cum = cums_name[[cc]])
    
    i <- i + 1
  }
}
df_plots

# merge
df_plots <- dplyr::bind_rows(df_plots) %>% 
  dplyr::mutate(fun = factor(fun, levels = funs_name),
                cum = factor(cum, levels = cums_name))

g3 <- df_plots %>% 
  ggplot(aes(x, y)) +
  geom_line() + 
  # inset_element(house, left = 0.01, right = 0.08, bottom = 0.02, top = 0.08) +
  facet_grid(cum~fun, labeller = label_parsed) +
  labs(x = "Space (km)", y = "Influence, \u03C6") +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  theme_classic(base_size = 14)
for(i in 1:length(points))
  g3 <- g3 + annotation_custom(grid::rasterGrob(house, interpolate = T),
                               xmin = (points[i] - 0.4), xmax = (points[i] + 0.4), 
                               ymin = -0.3, ymax = 0.3)
g3

# save
ggsave("zoi_cum_near.png", plot = g3, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

g_all <- ggpubr::ggarrange(g2, g3, nrow = 2, labels = c("A", "B"))

# save
ggsave("zoi_conceptual.png", plot = g_all, path = "text/figures/", width = 15, height = 20, 
       units = "cm", dpi = 300)


#--------------------------
# Impact

beta <- 2
zoi <- 3

impact_gaussian <- function(x, beta, zoi, zoi_decay_threshold) 
  beta*gaussian_decay(x, zoi = zoi, zoi_decay_threshold = zoi_decay_threshold)

# 1 feature
g4 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  # stat_function(fun = impact_gaussian, 
  #               args = list(beta = beta, zoi = zoi, zoi_decay_threshold = 0.05),
  #               # xlim = c(-2,0),
  #               geom = "area", fill = grey(0.6, alpha = 0.5)) +
  stat_function(fun = impact_gaussian,
                args = list(beta = beta, zoi = zoi, zoi_decay_threshold = 0.05),
                show.legend = FALSE) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.04, yend = 0.04, color = grey(0),
           size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", x = -3.5, xend = -3.5, y = 0, yend = 2, color = grey(0),
           size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 1.5, y = 0.09, color = grey(0), size = 5, 
           label = "Zone of Influence") +
  annotate("text", x = -3.8, y = 1, color = grey(0), size = 5, 
           label = "Effect size, \u03B2", angle = 90) +
  # annotate("text", x = 3.9, y = 0.81, color = grey(0), size = 7, 
  #          label = "Impact", ) +
  # geom_curve(
  #   aes(x = 1, y = 0.5, xend = 3, yend = 0.8),
  #   arrow = arrow(
  #     length = unit(0.03, "npc"), 
  #     type="closed" # Describes arrow head (open or closed)
  #   ),
  #   curvature = -0.3,  
  #   colour = "black",
  #   size = 1.2,
  #   angle = 90 # Anything other than 90 or 0 can look unusual
  # ) +
  scale_linetype_manual("Influence\nfunction", 
                        values = 1:4,
                        breaks = curves) +
  labs(x = "Distance, *d* (km)", y = "Impact") +
  mdthemes::md_theme_classic(base_size = 14) +
  inset_element(house, left = 0.47, right = 0.53, bottom = 0.01, top = 0.05)
g4  

plot_influence2d(0, zoi = 3, fun = gaussian_decay, range_plot = c(-5,5))
# plot_impact2d(0, beta, zoi)

# multiple features
g5 <- df_plots %>% 
  dplyr::filter(fun == "Gaussian") %>% 
  dplyr::mutate(cum = c("Cumulative", "Nearest")[cum]) %>% 
  ggplot(aes(x, beta*y)) +
  geom_line() + 
  # geom_ribbon(aes(x = x, ymax = beta*y), ymin = 0, fill = grey(0.6, alpha = 0.5)) +
  # inset_element(house, left = 0.01, right = 0.08, bottom = 0.02, top = 0.08) +
  facet_grid(cum~., labeller = label_parsed) +
  labs(x = "Spatial dimension (km)", y = "") +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  ylim(0, 4) +
  theme_classic(base_size = 14)
# for(i in 1:length(points))
#   g3 <- g3 + annotation_custom(grid::rasterGrob(house, interpolate = T),
#                                xmin = (points[i] - 0.4), xmax = (points[i] + 0.4), 
#                                ymin = -0.3, ymax = 0.3)
g5

g_all_impact <- ggpubr::ggarrange(g2, g4, g3, g5, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))

# save
ggsave("zoi_conceptual_impact.png", plot = g_all_impact, path = "text/figures/", width = 25, height = 20, 
       units = "cm", dpi = 300)

#---------------
# New nomenclature

# All curves
zoi <- 3
curves <- c("Threshold", "Linear", "Exponential", "Gaussian")
g1 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  stat_function(fun = exp_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Exponential")) +
  stat_function(fun = threshold_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Threshold")) +
  stat_function(fun = bartlett_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Linear")) +
  stat_function(fun = gaussian_decay, args = list(zoi = zoi, oneside = F, zoi_decay_threshold = 0.05), aes(linetype = "Gaussian")) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.03, yend = 0.03, color = grey(0.3),
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 1.5, y = 0.05, color = grey(0.3), size = 3, 
           label = "list(Radius, italic(r))", parse = T) +
  scale_linetype_manual("Shape", 
                        values = 1:4,
                        breaks = curves) +
  labs(x = "Distance, *d* (km)", y = "Zone of Influnce, \u03C6") +
  # theme_classic(base_size = 14) +
  mdthemes::md_theme_classic(base_size = 14) +
  inset_element(house, left = 0.47, right = 0.53, bottom = 0.01, top = 0.06) 
g1  

# save
ggsave("zoi_shapes_all_new.png", plot = g1, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

# remove exponential
zoi <- 3
g2 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  # stat_function(fun = exp_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Exponential")) +
  stat_function(fun = threshold_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Threshold")) +
  stat_function(fun = bartlett_decay, args = list(zoi = zoi, oneside = F), aes(linetype = "Linear")) +
  stat_function(fun = gaussian_decay, args = list(zoi = zoi, oneside = F, zoi_decay_threshold = 0.05), aes(linetype = "Gaussian")) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.03, yend = 0.03, color = grey(0.3),
           arrow = arrow(ends = "both", angle = 90, length = unit(.1,"cm"))) +
  annotate("text", x = 1.5, y = 0.05, color = grey(0.3), size = 3, 
           label = "list(Radius, italic(r))", parse = T) +
  scale_linetype_manual("Shape", 
                        values = 1:4,
                        breaks = curves) +
  labs(x = "Distance, *d* (km)", y = "Zone of Influnce, \u03C6") +
  ylim(0, 1) +
  # theme_classic(base_size = 14) +
  mdthemes::md_theme_classic(base_size = 14) +
  inset_element(house, left = 0.47, right = 0.53, bottom = 0.01, top = 0.06) 
g2

# save
ggsave("zoi_shapes_noexp_new.png", plot = g2, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

  #----------------------------------
# settings
points <- c(0.5, 4.5, 7, 8)
zoi <- 2.5

# functions
funs <- c("threshold_decay", "linear_decay", "gaussian_decay")
funs_name <- c("Threshold", "Linear", "Gaussian")
# cumulative
cums <- c(F, T)
# cums_name <- c("phi[nearest]", "phi[cumulative]")
cums_name <- c("Nearest", "Cumulative")

df_plots <- list()
i <- 1
for(ff in 1:length(funs)) {
  
  for(cc in 1:length(cums)) {
    
    df_plots[[i]] <- plot_influence2d(points, zoi, cumulative = cums[cc], 
                                      fun = funs[[ff]],
                                      range_plot = c(0, 12), return_df = TRUE)$influence_df %>% 
      dplyr::mutate(fun = funs_name[[ff]], cum = cums_name[[cc]])
    
    i <- i + 1
  }
}
df_plots

# merge
df_plots <- dplyr::bind_rows(df_plots) %>% 
  dplyr::mutate(fun = factor(fun, levels = funs_name),
                cum = factor(cum, levels = cums_name))

g3 <- df_plots %>% 
  ggplot(aes(x, y)) +
  geom_line() + 
  # inset_element(house, left = 0.01, right = 0.08, bottom = 0.02, top = 0.08) +
  facet_grid(cum~fun, labeller = label_parsed) +
  labs(x = "Space (km)", y = "Zone of Influence, \u03C6") +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  theme_classic(base_size = 14)
for(i in 1:length(points))
  g3 <- g3 + annotation_custom(grid::rasterGrob(house, interpolate = T),
                               xmin = (points[i] - 0.4), xmax = (points[i] + 0.4), 
                               ymin = -0.3, ymax = 0.3)
g3

# save
ggsave("zoi_cum_near_new.png", plot = g3, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

g_all <- ggpubr::ggarrange(g2, g3, nrow = 2, labels = c("A", "B"))

# save
ggsave("zoi_conceptual_new.png", plot = g_all, path = "text/figures/", width = 15, height = 20, 
       units = "cm", dpi = 300)


#--------------------------
# Impact

beta <- 2
zoi <- 3

impact_gaussian <- function(x, beta, zoi, zoi_decay_threshold) 
  beta*gaussian_decay(x, zoi = zoi, zoi_decay_threshold = zoi_decay_threshold)

# 1 feature
g4 <- ggplot(data.frame(x = c(-5, 5)), aes(x = x)) + 
  # stat_function(fun = impact_gaussian, 
  #               args = list(beta = beta, zoi = zoi, zoi_decay_threshold = 0.05),
  #               # xlim = c(-2,0),
  #               geom = "area", fill = grey(0.6, alpha = 0.5)) +
  stat_function(fun = impact_gaussian,
                args = list(beta = beta, zoi = zoi, zoi_decay_threshold = 0.05),
                show.legend = FALSE) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.04, yend = 0.04, color = grey(0),
           size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.1,"cm"))) +
  annotate("segment", x = -3.5, xend = -3.5, y = 0, yend = 2, color = grey(0),
           size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.1,"cm"))) +
  annotate("text", x = 1.5, y = 0.09, color = grey(0.3), size = 3, 
           label = "list(ZoI~radius, italic(r))", parse = T) +
  annotate("text", x = -3.8, y = 1, color = grey(0.3), size = 3, 
           label = "Effect size, \u03B2", angle = 90) +
  # annotate("text", x = 3.9, y = 0.81, color = grey(0), size = 7, 
  #          label = "Impact", ) +
  # geom_curve(
  #   aes(x = 1, y = 0.5, xend = 3, yend = 0.8),
  #   arrow = arrow(
  #     length = unit(0.03, "npc"), 
  #     type="closed" # Describes arrow head (open or closed)
  #   ),
  #   curvature = -0.3,  
  #   colour = "black",
  #   size = 1.2,
#   angle = 90 # Anything other than 90 or 0 can look unusual
# ) +
scale_linetype_manual("Influence\nfunction", 
                      values = 1:4,
                      breaks = curves) +
  labs(x = "Distance, *d* (km)", y = "Impact") +
  mdthemes::md_theme_classic(base_size = 14) +
  inset_element(house, left = 0.47, right = 0.53, bottom = 0.01, top = 0.05)
g4  

plot_influence2d(0, zoi = 3, fun = gaussian_decay, range_plot = c(-5,5))
# plot_impact2d(0, beta, zoi)

# multiple features
g5 <- df_plots %>% 
  dplyr::filter(fun == "Gaussian") %>% 
  dplyr::mutate(cum = c("Cumulative", "Nearest")[cum]) %>% 
  ggplot(aes(x, beta*y)) +
  geom_line() + 
  # geom_ribbon(aes(x = x, ymax = beta*y), ymin = 0, fill = grey(0.6, alpha = 0.5)) +
  # inset_element(house, left = 0.01, right = 0.08, bottom = 0.02, top = 0.08) +
  facet_grid(cum ~ ., labeller = label_parsed) +
  labs(x = "Space (km)", y = "Impact") +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  ylim(0, 4) +
  theme_classic(base_size = 14)
# for(i in 1:length(points))
#   g3 <- g3 + annotation_custom(grid::rasterGrob(house, interpolate = T),
#                                xmin = (points[i] - 0.4), xmax = (points[i] + 0.4), 
#                                ymin = -0.3, ymax = 0.3)
g5

g_all_impact <- ggpubr::ggarrange(g2, g4, g3, g5, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"))

# save
ggsave("zoi_conceptual_impact_new.png", plot = g_all_impact, path = "text/figures/", width = 25, height = 20, 
       units = "cm", dpi = 300)
