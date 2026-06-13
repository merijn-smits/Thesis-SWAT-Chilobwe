#plotting a 3x3 matrix 
library(tidyverse)
library(patchwork)

# ---- Scenario settings ----
scen_names <- c('SWC', 'climate_change', 'LUC', 'LUC_climate', 'combined')

scen_colors <- c(
  'SWC'            = '#E69F00',
  'climate_change' = '#56B4E9',
  'LUC'            = '#009E73',
  'LUC_climate'    = '#CC79A7',
  'combined'       = '#D55E00'
)
scen_labels <- c(
  'SWC'            = 'SWC',
  'climate_change' = 'Climate change',
  'LUC'            = 'LUC',
  'LUC_climate'    = 'LUC + Climate',
  'combined'       = 'Combined'
)

# ---- Variable metadata ----
# source : 'hru' or 'aqu'
# unit   : y-axis label
# flip   : negate values (WT: high value = deep water table, flip for intuition)
# label  : panel title
var_meta <- list(
  ET_mon_1 = list(unit = 'mm', flip = FALSE, label = 'Evapotranspiration'),
  SW_mon_1 = list(unit = 'mm', flip = FALSE, label = 'Soil water'),
  SQ_mon_1 = list(unit = 'mm', flip = FALSE, label = 'Surface runoff'),
  AQ_mon_1 = list(unit = 'mm', flip = FALSE, label = 'Aquifer outflow'),
  WT_mon_1 = list(unit = 'm',  flip = TRUE,  label = 'Groundwater level'),
  RC_mon_1 = list(unit = 'mm', flip = FALSE, label = 'Recharge')
)

# ---- Shared theme ----
base_theme <- theme_bw(base_size = 9) +
  theme(
    axis.title.x     = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = 'none'
  )

# ---- Helper: wide format data for HRU variables ----
build_data <- function(variable) {
  meta <- var_meta[[variable]]
  
  base_df <- results$baseline[[variable]] %>%
    select(date, baseline = run_1)
  
  scen_dfs <- lapply(scen_names, function(scen) {
    results[[scen]][[variable]] %>%
      select(date, run_1) %>%
      rename(!!scen := run_1)
  })
  
  dat <- base_df
  for (df in scen_dfs) dat <- inner_join(dat, df, by = 'date')
  
  if (meta$flip) {
    dat <- dat %>% mutate(across(c(baseline, all_of(scen_names)), ~ -.x))
  }
  dat
}
# ---- Helper: build one 2x3 panel figure ----
make_panel_figure <- function(var1, var2, fig_title) {
  plots <- list()
  
  for (variable in c(var1, var2)) {
    meta <- var_meta[[variable]]
    dat  <- build_data(variable)
    
    year_lines <- geom_vline(
      xintercept = as.numeric(seq(as.Date(paste0(format(min(dat$date), "%Y"), "-01-01")),
                                  as.Date(paste0(format(max(dat$date), "%Y"), "-01-01")),
                                  by = "year")),
      color = "grey90", linewidth = 0.2, linetype = "solid"
    )
    
    dat_long <- dat %>%
      pivot_longer(cols = all_of(scen_names), names_to = 'scenario', values_to = 'value') %>%
      mutate(
        abs_diff = value - baseline,
        pct_diff = ifelse(baseline == 0, NA_real_, (value - baseline) / baseline * 100),
        scenario = factor(scenario, levels = scen_names)
      )
    # Flip pct_diff sign for variables where values were negated (e.g. WT)
    if (meta$flip) {
      dat_long <- dat_long %>% mutate(pct_diff = -pct_diff)
    }
    
    # Row 1: baseline absolute values
    plots[[paste0(variable, '_r1')]] <- ggplot(dat, aes(x = date, y = baseline)) +
      geom_line(color = 'black', linewidth = 0.5) +
      year_lines +
      labs(title = meta$label, y = meta$unit) +
      base_theme +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
    
    # Row 2: absolute difference from baseline
    plots[[paste0(variable, '_r2')]] <- ggplot(dat_long, aes(x = date, y = abs_diff, color = scenario)) +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
      geom_line(linewidth = 0.5) +
      year_lines +
      scale_color_manual(values = scen_colors, labels = scen_labels, name = 'Scenario') +
      labs(y = paste0('\u0394 ', meta$unit)) +
      base_theme
    
    # Row 3: percent difference from baseline
    plots[[paste0(variable, '_r3')]] <- ggplot(dat_long, aes(x = date, y = pct_diff, color = scenario)) +
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
      geom_line(linewidth = 0.5) +
      year_lines +
      scale_color_manual(values = scen_colors, labels = scen_labels, name = 'Scenario') +
      labs(y = '\u0394 %') +
      base_theme
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

# ---- Produce and save the three figures ----
fig1 <- make_panel_figure('ET_mon_1', 'SW_mon_1', paste('Evapotranspiration & Soil water'))
fig2 <- make_panel_figure('AQ_mon_1', 'SQ_mon_1', paste('Returnflow & Surface runoff'))
fig3 <- make_panel_figure('WT_mon_1', 'RC_mon_1', paste('Groundwater level & Aquifer recharge'))

for (fig_info in list(
  list(fig = fig1, name = 'fig1_ET_SW'),
  list(fig = fig2, name = 'fig2_AQ_SQ'),
  list(fig = fig3, name = 'fig3_WT_RC')
)) {
  ggsave(
    paste0('./graphs/',fig_info$name, '.png'),
    plot   = fig_info$fig,
    width  = 297, height = 210,   # A4 landscape
    units  = 'mm', dpi = 300
  )
}
