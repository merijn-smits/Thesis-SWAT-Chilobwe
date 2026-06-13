
### updadting the original model with the calibrated values###
library(tidyverse)
library(SWATrunR)
library(SWATrunR)

#define location of the TxtInOut and load the calibrated parameters
baseline_path <- './Chilobwe_calibrated/Scenarios/Default/TxtInOut'
swc_path <- './Chilobwe_SWC/Scenarios/Default/TxtInOut'
climatec_path <- './Chilobwe_ClimateChange/Scenarios/Default/TxtInOut'
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
  "lat_ttime",  "hru", "relchg",
  "k",          "sol", "relchg",
  "perco",      "hru", "absval",
  "alpha",      "aqu", "absval",
  "revap_min",  "aqu", "abschg",
  "bf_max",     "aqu", "absval",
  "revap_co",   "aqu", "absval"
)

# Create the right column names for SWAT input. 
swat_params_wide <- cal_params %>%
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
cal_params <- swat_params_wide

#start and end dates
start_date <- '2016-01-01'
end_date <- '2024-12-01'
start_date_print <- '2018-01-01'


#define outputs
# Channel IDs for which simulation outputs are returned.
cha_ids <- c(9,8,7,6,5,3,20,21) # this is the outlet and all headwaters, close to the SWC
# HRU IDs for which simulation outputs are returned.i.e. all HRUs
hru_ids <- 1:305
 
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
  ET_mon = define_output(file = 'hru_wb_mon',
                         variable = 'et',
                         unit = hru_ids),
  
  # surq_gen :generated surface runoff
  SQ_mon = define_output(file = 'hru_wb_mon',
                         variable = 'surq_gen',
                         unit = hru_ids),
  
  # latq: lateral subsurface flow
  LQ_mon = define_output(file = 'hru_wb_mon',
                         variable = 'latq',
                         unit = hru_ids),
  
  # sw_final: soil water at the end of the month
  SW_mon = define_output(file = 'hru_wb_mon',
                         variable = 'sw_final',
                         unit = hru_ids),
  
  # perc: percolation to the aquifer
  PR_mon = define_output(file = 'hru_wb_mon',
                         variable = 'perc',
                         unit = hru_ids),
  
  # yield: maize yield per year
  YD_yr  = define_output(file = 'hru_pw_yr',
                         variable = 'yield',
                         unit = hru_ids),
  
  # flo: flow from shallow aquifer to the channel
  AQ_mon = define_output(file = 'aquifer_mon',
                         variable = 'flo',
                         unit = hru_ids),
  
  # dep_wt: depth to the watertable
  WT_mon = define_output(file = 'aquifer_mon',
                         variable = 'dep_wt',
                         unit = hru_ids),
  
  # revap: evaporation by capillary rise from the aquifer
  RE_mon = define_output(file = 'aquifer_mon',
                         variable = 'revap',
                         unit = hru_ids),
  
  # rchrg: recharge to aquifer
  RC_mon = define_output(file = 'aquifer_mon',
                         variable = 'rchrg',
                         unit = hru_ids)
)


baseline <- run_swatplus(project_path     = baseline_path,
                         output           = outputs,
                         parameter        = cal_params,
                         start_date       = start_date,
                         end_date         = end_date,
                         save_path        = './Chilobwe_calibrated/' ,
                         start_date_print = start_date_print,
                         save_file        = "baseline_cal",
                         split_units      = TRUE 
                         )


current_swc <- run_swatplus(project_path     = swc_path,
                            output           = outputs,
                            parameter        = cal_params,
                            start_date       = start_date,
                            end_date         = end_date,
                            save_path        = './Chilobwe_SWC/' ,
                            start_date_print = start_date_print,
                            save_file        = "current_swc_cal",
                            split_units      = TRUE
                            )
    
