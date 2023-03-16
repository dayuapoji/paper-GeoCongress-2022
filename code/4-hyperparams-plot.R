# ==============================================================================
# PLOT HYPERPARAMETER TUNING
# ==============================================================================

# setup
set_theme <- theme_bw(base_size = text_size) +
  theme(legend.key.size = unit(0.2, 'cm'),
        # legend.text = element_text(size=6),
        legend.position = c(.5, .3))

# ------------------------------------------------------------------------------
# GINI NTREES
# ------------------------------------------------------------------------------

gini_ntrees <- cv_results_list %>% 
  .[(.$splitrule == 'gini'), ] %>% 
  .[(.$min.node.size == 10), ]  

plot_gini_ntrees <- ggplot(data = gini_ntrees,
                           aes(x = ntrees, y = Accuracy, group = mtry)) +
  geom_line(aes(color = factor(mtry)), size = 0.25) +
  geom_point(aes(color = factor(mtry)), size = 0.25) +
  
  ggtitle("(a)") +
  scale_color_discrete(name = "Gini\nMin node size = 10\nMtry") +
  labs(x = "Ntrees", y = "OOB classification accuracy") +
  ylim(0.7, 1) +
  guides(color=guide_legend(ncol=3)) +
  
  set_theme 


# ------------------------------------------------------------------------------
# EXTRATREES NTREES
# ------------------------------------------------------------------------------

extra_ntrees <- cv_results_list %>% 
  .[(.$splitrule == 'extratrees'), ] %>% 
  .[(.$min.node.size == 10), ]  

plot_extra_ntrees <- ggplot(data = extra_ntrees,
                            aes(x = ntrees, y = Accuracy, group = mtry)) +
  geom_line(aes(color = factor(mtry)), size = 0.25) +
  geom_point(aes(color = factor(mtry)), size = 0.25) +
  
  ggtitle("(b)") +
  scale_color_discrete(name = "Extratrees\nMin node size = 10\nMtry") +
  labs(x = "Ntrees", y = "OOB classification accuracy") +
  ylim(0.7, 1) +
  guides(color=guide_legend(ncol=3)) +
  
  set_theme 

# ------------------------------------------------------------------------------
# COMBINE NTREES
# ------------------------------------------------------------------------------

plot_ntrees <- plot_grid(plot_gini_ntrees, plot_extra_ntrees, 
                         align = 'h', ncol = 2, 
                         rel_widths = c(0.5, 0.5))

# ------------------------------------------------------------------------------
# GINI MTRY
# ------------------------------------------------------------------------------

gini_mtry <- cv_results_list %>% 
  .[(.$splitrule == 'gini'), ] %>% 
  .[(.$ntrees == 500), ]

plot_gini_mtry <- ggplot(data = gini_mtry,
                         aes(x = mtry, y = Accuracy,
                             group = factor(min.node.size))) +
  geom_line(aes(color = factor(min.node.size)), size = 0.25) +
  geom_point(aes(color = factor(min.node.size)), size = 0.25) +
  
  ggtitle("(c)") +
  scale_color_discrete(name = "Gini\nNtrees = 500\nMin node size") +
  labs(x = "Mtry", y = "OOB classification accuracy") +
  ylim(0.92, 0.985) +
  guides(color=guide_legend(ncol=2)) +
  
  set_theme 

# ------------------------------------------------------------------------------
# EXTRATREES MTRY
# ------------------------------------------------------------------------------

extra_mtry <- cv_results_list %>% 
  .[(.$splitrule == 'extratrees'), ] %>% 
  .[(.$ntrees == 500), ]

plot_extra_mtry <- ggplot(data = extra_mtry,
                          aes(x = mtry, y = Accuracy, 
                              group = factor(min.node.size))) +
  geom_line(aes(color = factor(min.node.size)), size = 0.25) +
  geom_point(aes(color = factor(min.node.size)), size = 0.25) +
  
  ggtitle("(d)") +
  scale_color_discrete(name = "Extratrees\nNtrees = 500\nMin node size") +
  labs(x = "Mtry", y = "OOB classification accuracy") +
  ylim(0.92, 0.985) +
  guides(color=guide_legend(ncol=2)) +
  
  set_theme 

# ------------------------------------------------------------------------------
# COMBINE MTRY
# ------------------------------------------------------------------------------

plot_mtry <- plot_grid(plot_gini_mtry, plot_extra_mtry, 
                       align = 'h', ncol = 2, 
                       rel_widths = c(0.5, 0.5))

# ------------------------------------------------------------------------------
# COMBINE FIGURES
# ------------------------------------------------------------------------------

plot_hyperparam <- plot_grid(plot_ntrees, plot_mtry,
                             align = 'h', ncol = 2, 
                             rel_widths = c(0.5, 0.5))

# ------------------------------------------------------------------------------
# SAVE FIGURE
# ------------------------------------------------------------------------------

pdf(file = "../figures/hyperparam.pdf", 
    width= 7.2, height = 7.2/3.5,
    useDingbats = F) 
plot_hyperparam 
dev.off() 