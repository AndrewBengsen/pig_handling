# Figure 1: site map

# First load data frame x in `Initial analysis.qmd`

library(sf); library(ggspatial); library(ozmaps); library(ggplot2); library(cowplot)
library(ggmap); library(tidyverse)
library(fpc)
dsn <- "geographic/"
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
jama_palette3 <- c("#00a1d5", "#df8f44", "black")

theme_fig1 <- function () { 
  theme_bw(base_size = 9) +
    theme(plot.margin = margin(1,1,1,1, unit="pt"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          #panel.border = element_blank(),
          #panel.background = element_rect(fill="cornsilk1"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}
#theme_set(theme_fig1())  
theme_set(theme_classic())  
#nsw_map <- ozmap_states %>%
#  filter(NAME == "New South Wales")

ozmap <- ozmap(x="states")

sites <- st_read(dsn, "pig_clusters_4326") %>%
  mutate(method = factor(method, labels = c("A", "B", "C")))

site_cents <- st_centroid(sites) 

bbox_oz <- c(111.9, -44.5, 154.3, -10.1) #BL, TR
bbox_df <- data.frame(xlim = c(bbox_oz[1], bbox_oz[1], bbox_oz[3],   bbox_oz[3]),  # BL, TL, TR, BR
                      ylim = c(bbox_oz[2], bbox_oz[4], bbox_oz[4], bbox_oz[2]))

(FigMap <- ggplot() +
    geom_sf(colour="grey60", fill="grey80", data=ozmap) +
    geom_sf(aes(colour = method), size=2, data=site_cents) +
    scale_colour_manual(values = jama_palette3) +
    labs(colour = "Program") +
    coord_sf(xlim = c(bbox_oz[1], bbox_oz[3]), 
           ylim = c(bbox_oz[2], bbox_oz[4]),
           label_axes = "SW") +
    annotation_scale(bar_cols = c("black", "white"), width_hint=0.3, location="bl",
                     height = unit(0.15, "cm"), pad_y = unit(25, "pt"), pad_x = unit(10, "pt")) +
    annotation_north_arrow(location="tl", style=north_arrow_minimal) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size=10),
          legend.title = element_text(size=10)))

ggsave("output/Figure_1.png", FigMap, width = 9, height = 9, units = "cm")
