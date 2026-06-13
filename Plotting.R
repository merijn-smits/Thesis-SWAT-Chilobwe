# ---- Variable metadata ----
var_meta_swc <- list(
  ET_mon = list(unit = 'mm', label = 'Evapotranspiration'),
  SW_mon = list(unit = 'mm', label = 'Soil water'),
  PR_mon = list(unit = 'mm', label = 'Percolation'),
  SQ_mon = list(unit = 'mm', label = 'Surface runoff')
)

# ---- Colors for this script (2 scenarios only) ----
swc_colors <- c(
  'baseline'    = 'black',
  'current_swc' = '#E69F00'
)
swc_labels <- c(
  'baseline'    = 'Baseline',
  'current_swc' = 'SWC'
)

# ---- Helper: build wide format data for one variable ----
build_data_swc <- function(variable) {
  inner_join(
    result$baseline[[variable]],
    result$current_swc[[variable]],
    by = c('lu_mgt', 'date')
  ) %>%
    filter(lu_mgt == LU) %>%
    rename(baseline = ET.x, current_swc = ET.y)
}

# ---- Helper: build one 2x3 panel figure ----
make_swc_figure <- function(var1, var2, fig_title) {
  plots <- list()
  
  for (variable in c(var1, var2)) {
    meta <- var_meta_swc[[variable]]
    dat  <- build_data_swc(variable)
    
    # Year lines
    year_lines <- geom_vline(
      xintercept = as.numeric(seq(
        as.Date(paste0(format(min(dat$date), '%Y'), '-01-01')),
        as.Date(paste0(format(max(dat$date), '%Y'), '-01-01')),
        by = 'year')),
      color = 'grey70', linewidth = 0.2, linetype = 'solid'
    )
    
    # Long format for difference rows
    dat_long <- dat %>%
      pivot_longer(cols = 'current_swc', names_to = 'scenario', values_to = 'value') %>%
      mutate(
        abs_diff = value - baseline,
        pct_diff = ifelse(value > 1 | baseline > 1, (value - baseline) / baseline * 100, NA_real_)
      )
    
    # Long format for row 1 (both scenarios)
    dat_long_all <- dat %>%
      pivot_longer(cols = c('baseline', 'current_swc'), names_to = 'scenario', values_to = 'value') %>%
      mutate(scenario = factor(scenario, levels = c('baseline', 'current_swc')))
    
    # Row 1: actual values, both scenarios
    plots[[paste0(variable, '_r1')]] <- ggplot(dat_long_all, aes(date, value, color = scenario)) +
      year_lines +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = swc_colors, labels = swc_labels, name = 'Scenario') +
      labs(title = meta$label, y = meta$unit) +
      base_theme +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
    
    # Row 2: absolute difference
    plots[[paste0(variable, '_r2')]] <- ggplot(dat_long, aes(date, abs_diff, color = scenario)) +
      year_lines +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = swc_colors['current_swc'], labels = swc_labels['current_swc'], name = 'Scenario') +
      labs(y = paste0('\u0394 ', meta$unit)) +
      base_theme + 
      guides(color = 'none')
    
    # Row 3: percent difference
    plots[[paste0(variable, '_r3')]] <- ggplot(dat_long, aes(date, pct_diff, color = scenario)) +
      year_lines +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = swc_colors['current_swc'], labels = swc_labels['current_swc'], name = 'Scenario') +
      labs(y = '\u0394 %') +
      base_theme +
      guides(color = 'none')
  }
  
  # Suppress legend on all panels except first
  plots[[paste0(var2, '_r1')]] <- plots[[paste0(var2, '_r1')]] + guides(color = 'none')
  
  # Assemble 2x3 grid
  (plots[[paste0(var1, '_r1')]] | plots[[paste0(var2, '_r1')]]) /
    (plots[[paste0(var1, '_r2')]] | plots[[paste0(var2, '_r2')]]) /
    (plots[[paste0(var1, '_r3')]] | plots[[paste0(var2, '_r3')]]) +
    plot_layout(guides = 'collect') +
    plot_annotation(
      title   = fig_title,
      caption = 'Row 1: Absolute values  |  Row 2: Absolute difference from baseline  |  Row 3: Percent difference from baseline'
    ) &
    theme(
      legend.position = 'bottom',
      plot.title      = element_text(hjust = 0.5, size = 11, face = 'bold'),
      plot.caption    = element_text(hjust = 0.5, size = 8, color = 'grey40')
    )
}

# ---- Produce and save figures ----
fig_et_sw <- make_swc_figure('ET_mon', 'SW_mon', paste('ET & Soil water — Agricultural land with deep trenches'))
fig_pr_sq <- make_swc_figure('PR_mon', 'SQ_mon', paste('Percolation & Surface runoff — Agricultural land with deep trenches'))

for (fig_info in list(
  list(fig = fig_et_sw, name = 'swc_ET_SW'),
  list(fig = fig_pr_sq, name = 'swc_PR_SQ')
)) {
  ggsave(
    paste0('./graphs/', LU, '_', fig_info$name, '.png'),
    plot   = fig_info$fig,
    width  = 297, height = 210,
    units  = 'mm', dpi = 300
  )
} 