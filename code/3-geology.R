# ==============================================================================
# GEOLOGY
# ==============================================================================

# load geologic map
geomap <- readPNG('../../../../2-data/seattle/geomap.png', 
                  native = TRUE,
                  info = TRUE)

# ==============================================================================
# PLOT GEOLOGIC MAP
# ==============================================================================

plot_geomap <- ggplot() +
  annotation_raster(geomap,
                    xmin = 20000, xmax = 28800,
                    ymin = 0, ymax = 1) +
  
  # ggtitle("(a)") +
  ylim(0, 1) +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = geo_label$Chainage,
                     labels = geo_label$BH) +
  
  theme_classic(base_size = text_size) + 
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 5, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# ==============================================================================
# PLOT BOREHOLES
# ==============================================================================

plot_bh <- ggplot() + 
  geom_segment(data = rf_df,
               mapping =  aes(x = Chainage, xend = Chainage, 
                              y = 0, yend = 0.5,
                              color = SoilLabel),
               size=0.3, alpha=0.5) +
  scale_color_manual(values = colors) +
  
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  ylim(0, 1) +
  xlab('Chainage (ft)') +
  ylab('Soil Labels') +

  guides(color = guide_legend(nrow = 1)) +
  theme_classic(base_size = text_size) + 
  theme(legend.title = element_blank(), #element_text(size = 5),
        legend.position = c(.5, 0.9),
        legend.key.size = unit(0.3, 'cm'),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ==============================================================================
# COMBINE FIGURES
# ==============================================================================

plot_geo <- plot_grid(plot_geomap, plot_bh, align = 'v', nrow = 2,
                 rel_heights = c(0.65, 0.35))





