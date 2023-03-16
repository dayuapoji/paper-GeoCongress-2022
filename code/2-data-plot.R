# ==============================================================================
# PLOT DATA
# ==============================================================================

# setup
colors <- c('CCS'="blue",
            'CCS_CSG'="dodgerblue",
            'CSGCSF_CCS'= "sandybrown",
            'CSGCSF_TLTD'= 'pink2',
            'TLTD_CSGCSF_CCS'= "purple",
            'Unlabeled'='gray')

text_size <- 7

set_theme <- theme_bw(base_size = text_size) +
  theme(legend.key.size = unit(0.3, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# ------------------------------------------------------------------------------
# PLOT FORCES
# ------------------------------------------------------------------------------

plot_df <- rf_df[c('Ring', 'Chainage',
                   'ThrustForce', 'CutterTorque', 'CutterHeadForce')]

plot_force <- ggplot() + 
  geom_line(data = melt(plot_df, id.vars = c('Ring', 'Chainage')),
            mapping = aes(x = Chainage, y = value, color = variable),
            size = 0.25) +
  scale_color_brewer(palette = 'Set1', name = 'Features') +
  geom_rect(data = geo_label[(geo_label$SoilLabel != 'CCS_TLTD'), ],
            mapping = aes(xmin = ChainageStart, 
                          xmax = ChainageEnd, 
                          ymin = -Inf, ymax = Inf,
                          fill = SoilLabel),
            alpha = 0.1) +
  scale_fill_manual(values = colors, name = 'Soil Labels') +
  
  ggtitle("(a)") +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  xlab('Chainage (ft)') + 
  ylab('Force (kN), Torque (kNm)') +
  
  set_theme + theme(legend.key.height = unit(0.3, 'cm'))

# ------------------------------------------------------------------------------
# PLOT SPEED
# ------------------------------------------------------------------------------

colnames <- rf_df %>% select(contains('rot')|contains('speed')) %>% names(.)
plot_df <- rf_df[c('Ring', 'Chainage', colnames)]

plot_speed <- ggplot() +
  geom_line(data = melt(plot_df, id.vars = c('Ring', 'Chainage')),
            mapping = aes(x = Chainage, y = value, color = variable),
            size = 0.25) +
  scale_color_brewer(palette = 'Set1' , name = 'Features') +
  geom_rect(data = geo_label[(geo_label$SoilLabel != 'CCS_TLTD'), ],
            mapping = aes(xmin = ChainageStart, 
                          xmax = ChainageEnd, 
                          ymin = -Inf, ymax = Inf,
                          fill = SoilLabel),
            alpha = 0.1,
            show.legend = F) +
  scale_fill_manual(values = colors, name = 'Soil Labels') +
  
  ggtitle("(b)") +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  xlab('Chainage (ft)') +
  ylab('Advance Speed (mm/min),\nRotation Speed (rpm)') +
  
  set_theme 

# ------------------------------------------------------------------------------
# PLOT VOLUMES
# ------------------------------------------------------------------------------

colnames <- rf_df %>% select(contains('vol')) %>% names(.)
plot_df <- rf_df[c('Ring', 'Chainage', colnames)]

plot_vol <- ggplot() + 
  geom_line(data = melt(plot_df, id.vars = c('Ring', 'Chainage')),
            mapping = aes(x = Chainage, y = value, color = variable),
            size = 0.25) +
  scale_color_brewer(palette = 'Set3' , name = 'Features') +
  geom_rect(data = geo_label[(geo_label$SoilLabel != 'CCS_TLTD'), ],
            mapping = aes(xmin = ChainageStart, 
                          xmax = ChainageEnd, 
                          ymin = -Inf, ymax = Inf,
                          fill = SoilLabel),
            alpha = 0.1,
            show.legend = F) +
  scale_fill_manual(values = colors, name = 'Soil Label') +
  
  ggtitle("(c)") +
  scale_x_continuous(limit = c(20000, 28800),
                     breaks = seq(20000, 28800, 500)) +
  xlab('Chainage (ft)') +
  ylim(0, 600) +
  ylab('Volume (m3/m)') +
  
  set_theme

# ------------------------------------------------------------------------------
# COMBINE FIGURES
# ------------------------------------------------------------------------------

plot_eda <- plot_grid(plot_force, plot_speed, plot_vol,
                      align = 'v', nrow = 3, 
                      rel_heights = c(1/3, 1/3, 1/3))

# ------------------------------------------------------------------------------
# SAVE FIGURES
# ------------------------------------------------------------------------------

pdf(file = "../figures/eda.pdf", 
    width= 7.2, height = 7.2/1.5, 
    useDingbats = F) 
plot_eda 
dev.off() 