#### PLOTTING DISCHARGE DATA AND FDC'S ####

# ---- Prepare discharge data ----
# Wide format for difference calculations
Q_wide <- Q_df %>%
  select(scenario, date, run_1) %>%
  pivot_wider(names_from = scenario, values_from = run_1)

Q_long_diff <- Q_wide %>%
  pivot_longer(cols = all_of(scen_names), names_to = 'scenario', values_to = 'value') %>%
  mutate(
    abs_diff = value - baseline,
    pct_diff = ifelse(baseline == 0, NA_real_, (value - baseline) / baseline * 100),
    scenario = factor(scenario, levels = scen_names)
  )

# ---- Prepare FDC data ----
fdc_wide <- fdc_df %>%
  select(scenario, p, run_1) %>%
  pivot_wider(names_from = scenario, values_from = run_1)

fdc_long_diff <- fdc_wide %>%
  pivot_longer(cols = all_of(scen_names), names_to = 'scenario', values_to = 'value') %>%
  mutate(
    abs_diff = value - baseline,
    pct_diff = ifelse(baseline == 0, NA_real_, (value - baseline) / baseline * 100),
    scenario = factor(scenario, levels = scen_names)
  )

# ---- Year lines for discharge panels ----
year_lines <- geom_vline(
  xintercept = as.numeric(seq(
    as.Date(paste0(format(min(Q_df$date), "%Y"), "-01-01")),
    as.Date(paste0(format(max(Q_df$date), "%Y"), "-01-01")),
    by = "year")),
  color = "grey70", linewidth = 0.2, linetype = "solid"
)

# ---- Shared color scale ----
color_scale_scen <- scale_color_manual(
  values = scen_colors,
  labels = scen_labels,
  name   = 'Scenario'
)
color_scale_all <- scale_color_manual(
  values = c(baseline = 'black', scen_colors),
  labels = c(baseline = 'Baseline', scen_labels),
  name   = 'Scenario'
)

# ---- Row 1: Actual values ----
p_q_actual <- ggplot(Q_df, aes(date, run_1, color = scenario)) +
  year_lines +
  geom_line(linewidth = 0.5) +
  color_scale_all +
  labs(title = 'Monthly discharge', y = 'm³/s') +
  base_theme +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))


p_fdc_actual <- ggplot(fdc_df, aes(p, run_1, color = scenario)) +
  geom_line(linewidth = 0.5) +
  color_scale_all +
  labs(y = 'm³/s ', x = 'Exceedance probability', title = 'Flow duration curve') +
  base_theme +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# ---- Row 2: Absolute difference ----
p_q_abs <- ggplot(Q_long_diff, aes(date, abs_diff, color = scenario)) +
  year_lines +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
  geom_line(linewidth = 0.5) +
  color_scale_scen +
  labs(y = '\u0394 m³/s') +
  base_theme

p_fdc_log <- ggplot(fdc_df, aes(p, run_1, color = scenario)) +
  geom_line(linewidth = 0.5) +
  scale_y_log10() +
  color_scale_all +
  labs(y = 'm³/s (log)', x = 'Exceedance probability') +
  base_theme +
  theme(axis.title.x = element_text())

# ---- Row 3: Relative difference ----
p_q_pct <- ggplot(Q_long_diff, aes(date, pct_diff, color = scenario)) +
  year_lines +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
  geom_line(linewidth = 0.5) +
  color_scale_scen +
  labs(y = '\u0394 %') +
  base_theme

p_fdc_pct <- ggplot(fdc_long_diff, aes(p, pct_diff, color = scenario)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey40', linewidth = 0.4) +
  geom_line(linewidth = 0.5) +
  color_scale_scen +
  labs(y = '\u0394 %', x = 'Exceedance probability') +
  base_theme +
  theme(axis.title.x = element_text())

# ---- Assemble 2x3 grid ----
# Suppress legend on every panel except p_q_actual
p_fdc_actual  <- p_fdc_actual  + guides(color = 'none')
p_q_abs       <- p_q_abs       + guides(color = 'none')
p_fdc_log     <- p_fdc_log     + guides(color = 'none')
p_q_pct       <- p_q_pct       + guides(color = 'none')
p_fdc_pct     <- p_fdc_pct     + guides(color = 'none')

# Now assemble — only p_q_actual has a legend, collect it to bottom
Q_panel <- (p_q_actual | p_fdc_actual) /
  (p_q_abs    | p_fdc_log)    /
  (p_q_pct    | p_fdc_pct)    +
  plot_layout(guides = 'collect') +
  plot_annotation(
    caption = 'Row 1: Absolute values  |  Row 2: Absolute difference / Log FDC  |  Row 3: Relative difference'
  ) &
  theme(
    legend.position = 'bottom',
    plot.caption    = element_text(hjust = 0.5, size = 8, color = 'grey40')
  )
# ---- Save ----
ggsave(
  './graphs/Q_discharge_panel.png',
  plot   = Q_panel,
  width  = 297, height = 210,
  units  = 'mm', dpi = 300
)