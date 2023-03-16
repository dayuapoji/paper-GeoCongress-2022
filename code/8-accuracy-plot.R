# ==============================================================================
# PLOT ACCURACY
# ==============================================================================

plot_accuracy <- ggplot(data = accuracy_feature,
                        aes(x = Feature, y = Accuracy)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = AccuracyLower, 
                              ymax = AccuracyUpper),
                width = 0.5, size = 0.5) +
  ylim(0, 1) +
  ylab('Classification Accuracy') +
  theme_bw(base_size = text_size) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# ------------------------------------------------------------------------------
# SAVE FIGURE
# ------------------------------------------------------------------------------
pdf(file = "../figures/accuracy.pdf", 
    width= 7.2, height = fig_size/2,
    useDingbats=F) 
plot_accuracy 
dev.off() 
