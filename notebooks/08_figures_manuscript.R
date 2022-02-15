# code for figures
library(oneimpact)
library(dplyr)
library(ggplot2)
library(patchwork)
library(png)

house <- jpeg::readJPEG("text/figures/house_drawing.jpg", native = TRUE)

# All curves
zoi <- 3
curves <- c("Threshold", "Linear", "Exponential", "Gaussian")
g1 <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) + 
  stat_function(fun = exp_decay, args = list(zoi = zoi), aes(linetype = "Exponential")) +
  stat_function(fun = threshold_decay, args = list(zoi = zoi), aes(linetype = "Threshold")) +
  stat_function(fun = bartlett_decay, args = list(zoi = zoi), aes(linetype = "Linear")) +
  stat_function(fun = gaussian_decay, args = list(zoi = zoi, zoi_decay_threshold = 0.05), aes(linetype = "Gaussian")) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.1, yend = 0.1, color = grey(0.5),
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 1.5, y = 0.07, color = grey(0.5), size = 3, label = "Zone of Influence") +
  scale_linetype_manual("Influence\nfunction", 
                        values = 1:4,
                        breaks = curves) +
  labs(x = "Distance (km)", y = "Influence") +
  theme_classic(base_size = 14) +
  inset_element(house, left = 0.01, right = 0.08, bottom = 0.02, top = 0.08) 
g1  

# save
ggsave("zoi_shapes_all.png", plot = g1, path = "text/figures/", width = 15, height = 10, 
       units = "cm", dpi = 300)

# remove exponential
zoi <- 3
g2 <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) + 
  stat_function(fun = threshold_decay, args = list(zoi = zoi), aes(linetype = "Threshold")) +
  stat_function(fun = bartlett_decay, args = list(zoi = zoi), aes(linetype = "Linear")) +
  stat_function(fun = gaussian_decay, args = list(zoi = zoi, zoi_decay_threshold = 0.05), aes(linetype = "Gaussian")) +
  annotate("segment", x = 0, xend = zoi-0.05, y = 0.1, yend = 0.1, color = grey(0.5),
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 1.5, y = 0.07, color = grey(0.5), size = 3, label = "Zone of Influence") +
  scale_linetype_manual("Influence\nfunction", values = 1:3, breaks = curves[-3]) +
  labs(x = "Distance (km)", y = "Influence") +
  theme_classic(base_size = 14) +
  inset_element(house, left = 0.01, right = 0.08, bottom = 0.02, top = 0.08) 
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
  facet_grid(cum~fun) +
  labs(x = "Distance (km)", y = "Influence") +
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
