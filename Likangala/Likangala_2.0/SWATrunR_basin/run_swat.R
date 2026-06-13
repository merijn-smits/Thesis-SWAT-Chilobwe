
### updadting the original model with the calibrated values###
library(tidyverse)
library(SWATrunR)
library(SWATrunR)

#define location of the TxtInOut and load the calibrated parameters
baseline_path <- '../Likangala_2.0/Scenarios/Default/TxtInOut'
swc_path <- '../Likangala_2.0_SWC/Scenarios/Default/TxtInOut'
climate_path <- '../Likangala_2.0_climate_change/Scenarios/Default/TxtInOut'
LUC_path <- '../Likangala_2.0_LUC/Scenarios/luc_only/TxtInOut'
LUC_climate_path  <- '../Likangala_2.0_LUC/Scenarios/luc_and_climate/TxtInOut'
combined_path <- '../Likangala_2.0_combined/Scenarios/default/TxtInOut'

cal_params <- read.csv('./calibrated_pars.csv')[1:3]


 #map the parameters, file extensions and the change type
param_map <- tibble::tribble(
  ~parameter,   ~file, ~change,
  "esco",       "hru", "absval",
  "awc",        "sol", "relchg",
  "canmx",      "hru", "absval",
  "cn2",        "hru", "relchg",
  "cn3_swf",    "hru", "absval",
  "latq_co",    "hru", "absval",
  "bd",         "sol", "relchg",
  "lat_ttime",  "hru", "absval",
  "k",          "sol", "relchg", 
  "perco",      "hru", "absval",
  "alpha",      "aqu", "absval",
  "revap_min",  "aqu", "absval",
  "bf_max",     "aqu", "absval",
  "revap_co",   "aqu", "absval",
  "flo_min",    "aqu", "absval"
)

# Create the right column names for SWAT input. 
swat_params <- cal_params %>%
  pivot_longer(
    cols = -landuse,
    names_to = "parameter",
    values_to = "value"
  ) %>%
  left_join(param_map, by = "parameter") %>%
  mutate(
    swat_name = paste0(
      parameter, ".", file, "_", landuse,
      "::", parameter, ".", file,
      " | change = ", change,
      " | landuse == ", landuse
    )
  ) %>%
  select(swat_name, value) %>%
  pivot_wider(
    names_from  = swat_name,
    values_from = value
  )


#start and end dates
start_date <- '2016-01-01'
end_date <- '2024-12-01'
start_date_print <- '2018-01-01'

# saveRDS(cal_params,paste0(scenarios$baseline$path,'.rds'))

#define outputs
# Channel IDs for which simulation outputs are returned.
cha_ids <- c(105,267)
# HRU IDs for which simulation outputs are returned. E.g. all HRUs
hru_ids <- 1:4011
 
# Output definition -------------------------------------------------------
outputs <- list(
  # Daily discharge
  flo_day = define_output(file = 'channel_sd_day',
                          variable = 'flo_out',
                          unit = cha_ids),
  #monthly discharge
  flo_mon = define_output(file = 'channel_sd_mon',
                          variable = 'flo_out',
                          unit = cha_ids),
  # Monthly ET
  ET_mon = define_output(file = 'basin_wb_mon',
                         variable = 'et',
                         unit = hru_ids),
  
  # surq_gen :generated surface runoff
  SQ_mon = define_output(file = 'basin_wb_mon',
                         variable = 'surq_gen',
                         unit = hru_ids),
  
  # latq: lateral subsurface flow
  LQ_mon = define_output(file = 'basin_wb_mon',
                         variable = 'latq',
                         unit = hru_ids),
  
  # sw_final: soil water at the end of the month
  SW_mon = define_output(file = 'basin_wb_mon',
                         variable = 'sw_final',
                        unit = hru_ids),
  
  # perc: percolation to the aquifer
  PR_mon = define_output(file = 'basin_wb_mon',
                         variable = 'perc',
                         unit = hru_ids),
  
  # yield: maize yield per year
  YD_yr  = define_output(file = 'basin_pw_yr',
                         variable = 'yield',
                         unit = hru_ids),
  
  # flo: flow from shallow aquifer to the channel
  AQ_mon = define_output(file = 'basin_aqu_mon',
                         variable = 'flo',
                         unit = hru_ids),
  
  # dep_wt: depth to the watertable
  WT_mon = define_output(file = 'basin_aqu_mon',
                         variable = 'dep_wt',
                         unit = hru_ids),
  
  # revap: evaporation by capillary rise from the aquifer
  RE_mon = define_output(file = 'basin_aqu_mon',
                         variable = 'revap',
                         unit = hru_ids),
  
  # rchrg: recharge to aquifer
  RC_mon = define_output(file = 'basin_aqu_mon',
                         variable = 'rchrg',
                         unit = hru_ids)
  
)


baseline <- run_swatplus(project_path     = baseline_path,
                         output           = outputs,
                         parameter        = swat_params,
                         start_date       = start_date,
                         end_date         = end_date,
                         save_path        = './' ,
                         start_date_print = start_date_print,
                         save_file        = paste0("Baseline_",format(Sys.time(), '%Y%m%d%H%M')),
                         split_units      = TRUE,
                         n_thread         = 8
                         )


    only_swc    <- run_swatplus(project_path     = swc_path,
                            output           = outputs,
                            parameter        = swat_params,
                            start_date       = start_date,
                            end_date         = end_date,
                            save_path        = './' ,
                            start_date_print = start_date_print,
                            save_file        = paste0("SWC_",format(Sys.time(), '%Y%m%d%H%M')),
                            split_units      = TRUE,
                            n_thread         = 8
                            )

climate_change <- run_swatplus(project_path     = climate_path,
                               output           = outputs,
                               parameter        = swat_params,
                               start_date       = start_date,
                               end_date         = end_date,
                               save_path        = './' ,
                               start_date_print = start_date_print,
                               save_file        = paste0("climate_change_",format(Sys.time(), '%Y%m%d%H%M')),
                               split_units      = TRUE
                               )
    
LU_change <-      run_swatplus(project_path     = LUC_path,
                               output           = outputs,
                               parameter        = swat_params,
                               start_date       = start_date,
                               end_date         = end_date,
                               save_path        = './' ,
                               start_date_print = start_date_print,
                               save_file        = paste0("LUC_",format(Sys.time(), '%Y%m%d%H%M')),
                               split_units      = TRUE
)

LU_climate <-     run_swatplus(project_path     = LUC_climate_path,
                               output           = outputs,
                               parameter        = swat_params,
                               start_date       = start_date,
                               end_date         = end_date,
                               save_path        = './' ,
                               start_date_print = start_date_print,
                               save_file        = paste0("LUC_climate_",format(Sys.time(), '%Y%m%d%H%M')),
                               split_units      = TRUE
)

 combined <-       run_swatplus(project_path    = combined_path,
                               output           = outputs,
                               parameter        = swat_params,
                               start_date       = start_date,
                               end_date         = end_date,
                               save_path        = './' ,
                               start_date_print = start_date_print,
                               save_file        = paste0("combined_",format(Sys.time(), '%Y%m%d%H%M')),
                               split_units      = TRUE
)
     