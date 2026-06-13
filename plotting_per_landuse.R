library(tidyverse)
library(patchwork)
# ---- Land use settings ----
lu_codes <- c('agdt', 'rndt', 'urdt', 'frdt')

lu_colors <- c(
  'agdt' = '#E69F00',
  'rndt' = '#009E73',
  'urdt' = '#CC79A7',
  'frdt' = '#56B4E9'
)
lu_labels <- c(
  'agdt' = 'Agricultural',
  'rndt' = 'Rangeland',
  'urdt' = 'Urban',
  'frdt' = 'Forest'
)

var_meta_swc <- list(
  ET_mon = list(unit = 'mm', label = 'Evapotranspiration'),
  SW_mon = list(unit = 'mm', label = 'Soil water'),
  PR_mon = list(unit = 'mm', label = 'Percolation'),
  SQ_mon = list(unit = 'mm', label = 'Surface runoff')
)

base_theme <- theme_bw(base_size = 9) +
  theme(
    axis.title.x     = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = 'none'
  )

# ---- Helper: build data for one variable, all land uses ----
build_data_swc <- function(variable) {
  inner_join(
    result$baseline[[variable]],
    result$current_swc[[variable]],
    by = c('lu_mgt', 'date')
  ) %>%
    filter(lu_mgt %in% lu_codes) %>%
    rename(baseline = ET.x, current_swc = ET.y) %>%
    mutate(
      abs_diff = current_swc - baseline,
      pct_diff = ifelse(current_swc > 2 | baseline > 2,
                        (current_swc - baseline) / baseline * 100,
                        NA_real_),
      lu_mgt = factor(lu_mgt, levels = lu_codes)
    )
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
    
    # Shared color scale
    color_scale <- scale_color_manual(
      values = lu_colors,
      labels = lu_labels,
      name   = 'Land use'
    )
    
    # Row 1: baseline only, all land uses
    plots[[paste0(variable, '_r1')]] <- ggplot(dat, aes(date, baseline, color = lu_mgt)) +
      year_lines +
      geom_line(linewidth = 0.5) +
      color_scale +
      labs(title = meta$label, y = meta$unit) +
      base_theme +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
    
    # Row 2: absolute difference, all land uses
    plots[[paste0(variable, '_r2')]] <- ggplot(dat, aes(date, abs_diff, color = lu_mgt)) +
      year_lines +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
      geom_line(linewidth = 0.5) +
      color_scale +
      labs(y = paste0('\u0394 ', meta$unit)) +
      base_theme
    
    # Row 3: percent difference, all land uses
    plots[[paste0(variable, '_r3')]] <- ggplot(dat, aes(date, pct_diff, color = lu_mgt)) +
      year_lines +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
      geom_line(linewidth = 0.5) +
      color_scale +
      labs(y = '\u0394 %') +
      base_theme
  }
  
  # Suppress legend on all panels except top-left
  for (nm in names(plots)) {
    if (nm != paste0(var1, '_r1')) {
      plots[[nm]] <- plots[[nm]] + guides(color = 'none')
    }
  }
  
  # Assemble 2x3 grid
  (plots[[paste0(var1, '_r1')]] | plots[[paste0(var2, '_r1')]]) /
    (plots[[paste0(var1, '_r2')]] | plots[[paste0(var2, '_r2')]]) /
    (plots[[paste0(var1, '_r3')]] | plots[[paste0(var2, '_r3')]]) +
    plot_layout(guides = 'collect') +
    plot_annotation(
      title   = fig_title,
      caption = 'Row 1: Baseline  |  Row 2: Absolute difference from baseline  |  Row 3: Percent difference from baseline'
    ) &
    theme(
      legend.position = 'bottom',
      plot.title      = element_text(hjust = 0.5, size = 11, face = 'bold'),
      plot.caption    = element_text(hjust = 0.5, size = 8, color = 'grey40')
    )
}

# ---- Produce and save figures ----
fig_et_sw <- make_swc_figure('ET_mon', 'SW_mon', 'Effects of SWC application')
fig_pr_sq <- make_swc_figure('PR_mon', 'SQ_mon', 'Effects of SWC application')

for (fig_info in list(
  list(fig = fig_et_sw, name = 'swc_ET_SW'),
  list(fig = fig_pr_sq, name = 'swc_PR_SQ')
)) {
  ggsave(
    paste0('./graphs/', fig_info$name, '.png'),
    plot   = fig_info$fig,
    width  = 297, height = 210,
    units  = 'mm', dpi = 300
  )
}
