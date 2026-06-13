
### updadting the original model with the calibrated values###
library(SWATreadR)
library(tidyverse)
library(SWATrunR)
library(SWATrunR)

#select landuse to update
lu_cals <- c('agrl','urbn','rnge','frst')

#define location of the TxtInOut
baseline_path <- '../Chilobwe_3.0/Scenarios/Default/TxtInOut'
swc_path <- '../'

#read csv and select the nr. 1 ranked calibration result

# 
# hyd  <- read_swat(paste0(model_path,'/hydrology.hyd'))
# sol  <- read_swat(paste0(model_path,'/soils.sol'))
# lum  <- read_swat(paste0(model_path,'/landuse.lum'))
par_update <- list()
#loop through landuse results

for (lu_cal in lu_cals) {
  calval <- read.csv(paste0('../results/calval_', lu_cal,'.csv')) %>%
    filter(rank == 1)
  
  par_update[[length(par_update)+1]] <- tibble(
    landuse = lu_cal,
    parameter = c("esco.hru", "cn2.hru", "cn3_swf.hru",
                  "latq_co.hru", "bd.sol", "perco.hru"),
    change   = c("absval", "relchg", "absval", "absval", "relchg", "absval"),
    value    = c(calval$esco, calval$cn2, calval$cn3_swf,
                 calval$latq_co, calval$bd, calval$perco)
  )
}

par_vals <- bind_rows(par_update)

par_vals <- par_vals %>%
  mutate(
    swat_string = paste0(parameter,'_',landuse,'::',
      parameter, " | change = ", change, " | landuse == ", landuse
    )
  )
calibrated <- t(par_vals)
colnames(calibrated) <- calibrated[5,]
calibrated <- as_tibble(calibrated)[4,]
calibrated <- calibrated %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

write_csv(calibrated,'../calibrated_params.csv')

baseline <- run_swatplus(project_path     = model_path,
                         output           = outputs,
                         parameter        = calibrated,
                         start_date       = start_date,
                         end_date         = end_date,
                         save_path        = save_path,
                         save_file        = "calibrated_2",
                         split_units      = TRUE, # better set TRUE for large number of units
)
