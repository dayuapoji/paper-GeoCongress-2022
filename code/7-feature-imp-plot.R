# ==============================================================================
# PLOT FEATURE IMPORTANCE
# ==============================================================================

# setup
set_theme <- theme_bw(base_size = text_size) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.title.y = element_blank(),
        legend.position = "none")

# ------------------------------------------------------------------------------
# PLOT IMPURITY
# ------------------------------------------------------------------------------

plot_impurity <- ggplot() +
  geom_bar(data = varimp_df,
           mapping = aes(x = Impurity, 
                         y = reorder(Features, Impurity),
                         fill = Groups),
           color = 'white',
           stat = 'identity') +
  scale_fill_brewer(palette="Set3") +
  xlab("Relative Importance") +
  ggtitle("(a)") +
  set_theme

# ------------------------------------------------------------------------------
# PLOT PERMUTATION
# ------------------------------------------------------------------------------

plot_permutation <- ggplot() +
  geom_bar(data = varimp_df,
           mapping = aes(x = Permutation, 
                         y = reorder(Features, Permutation),
                         fill = Groups),
           color = 'white',
           stat = 'identity') +
  scale_fill_brewer(palette="Set3") +
  xlab("Relative Importance") +
  ggtitle("(b)") +
  set_theme

# ------------------------------------------------------------------------------
# PLOT CONDITIONAL PERMUTATION
# ------------------------------------------------------------------------------

plot_conditional <-  ggplot() +
  geom_bar(data = varimp_df,
           mapping = aes(x = ConditionalForests, 
                         y = reorder(Features, ConditionalForests),
                         fill = Groups),
           color = 'white',
           stat = 'identity') +
  scale_fill_brewer(palette="Set3") +
  xlab("Relative Importance") +
  ggtitle("(c)") +
  set_theme

# ------------------------------------------------------------------------------
# COMBINE FIGURES
# ------------------------------------------------------------------------------

plot_importance <- plot_grid(plot_impurity, plot_permutation, plot_conditional, 
                             align = 'h', ncol = 3, 
                             rel_widths = c(1/3, 1/3, 1/3))

# ------------------------------------------------------------------------------
# SAVE FIGURE
# ------------------------------------------------------------------------------

pdf(file = "../figures/importance.pdf", 
    width= 7.2, height = fig_size/1.5, 
    useDingbats=F) 
plot_importance
dev.off()