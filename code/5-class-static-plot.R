# ==============================================================================
# PLOT STATIC CLASSIFICATION
# ==============================================================================

# setup
set_theme <- theme_bw(base_size = text_size) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ------------------------------------------------------------------------------
# PLOT CLASSIFICATION
# ------------------------------------------------------------------------------

class_static <- ggplot() +
  geom_segment(data = rf_results,
               mapping =  aes(x = Chainage, xend = Chainage,
                              y = 0, yend = 1,
                              color = Prediction),
               size=0.3, alpha=0.5) +
  geom_segment(data = rf_results,
               mapping =  aes(x = Chainage, xend = Chainage, 
                              y = 0, yend = Error), 
               size = 0.3) + 
  scale_color_manual(values = colors) +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  
  # ggtitle("(b)") +
  ylab('Class') +
  
  set_theme + theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank())

# ------------------------------------------------------------------------------
# PLOT PROBABILITY
# ------------------------------------------------------------------------------

prob_static <- ggplot(data = melt(rf_results[ , c(2, 4:8)], id.vars = 'Chainage'),
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

plot_class_static <- plot_grid(class_static, prob_static, align = 'v', nrow = 2, 
                               rel_heights = c(0.35, 0.65))
