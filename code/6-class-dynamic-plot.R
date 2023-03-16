# ==============================================================================
# PLOT DYNAMIC CLASSFICATION
# ==============================================================================

set_theme <- theme_bw(base_size = text_size) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ------------------------------------------------------------------------------
# PLOT CLASSIFICATION
# ------------------------------------------------------------------------------

class_dynamic <- ggplot() +
  geom_segment(data = rf_results_list,
               mapping =  aes(x = Chainage, xend = Chainage,
                              y = 0, yend = 1,
                              color = Prediction),
               size=0.3, alpha=0.5) +
  geom_segment(data = rf_results_list,
               mapping =  aes(x = Chainage, xend = Chainage, 
                              y = 0, yend = Error), 
               size = 0.3) + 
  scale_color_manual(values = colors) +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  
  # ggtitle("(c)") +
  ylab('Class') +
  
  set_theme + theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank())

# ------------------------------------------------------------------------------
# PLOT PROBABILITY
# ------------------------------------------------------------------------------

prob_dynamic <- ggplot(data = melt(rf_results_list[ , c(2, 4:8)], 
                                   id.vars = 'Chainage'),
                       mapping = aes(x = Chainage, y = value, fill = variable)) +
  geom_bar(stat = 'identity', width = 8, alpha = 0.8) +
  
  scale_fill_manual(values = colors) +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  
  xlab('Chainage (ft)') +
  ylab('Probability') +
  
  set_theme

# ------------------------------------------------------------------------------
# COMBINE FIGURES
# ------------------------------------------------------------------------------

plot_class_dynamic <- plot_grid(class_dynamic, prob_dynamic, align = 'v', nrow = 2, 
                                rel_heights = c(0.35, 0.65))

# ------------------------------------------------------------------------------
# COMBINE FIGURES: GEO + ALL MODELS
# ------------------------------------------------------------------------------

plot_class <- plot_grid(plot_geo, plot_class_static, plot_class_dynamic, 
                        align = 'v', nrow = 3, rel_heights = c(0.5, 0.25, 0.25))

# ------------------------------------------------------------------------------
# SAVE FIGURE
# ------------------------------------------------------------------------------

pdf(file = "../figures/class.pdf", 
    width = 7.2, height = 7.2/1.2,
    useDingbats = F)
plot_class
dev.off()

################################################################################
# FIGS FOR DISSERTATION

pdf(file = "../figs/fig-07-08.pdf", 
    width = 7.5, height = 7.5/1.25,
    useDingbats = F)
plot_grid(plot_geo, plot_class_static, 
          align = 'v', nrow = 2, rel_heights = c(0.6, 0.4))
dev.off()

pdf(file = "../figs/fig-07-09.pdf", 
    width = 7.5, height = 7.5/1.25,
    useDingbats = F)
plot_grid(plot_geo,  plot_class_dynamic, 
                   align = 'v', nrow = 2, rel_heights =c(0.6, 0.4))
dev.off()