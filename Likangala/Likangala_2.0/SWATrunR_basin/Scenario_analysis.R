#Analysis script to compare the different results
library(tidyverse)
library(SWATtunR)
library(SWATrunR)
source('HRU_processing.r')
source('AQU_processing.r')


#file locations
scenarios_paths <- list(
  baseline = list(
    path = list.files(pattern = "^Baseline" ),
    hru = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0/Scenarios/Default/TxtInOut/hru-data.hru",
    con = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0/Scenarios/Default/TxtInOut/hru.con"
  ),
  SWC = list(
    path = list.files(pattern = "^SWC"),
    hru = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_SWC/Scenarios/Default/TxtInOut/hru-data.hru",
    con = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_SWC/Scenarios/Default/TxtInOut/hru.con"
  ),
  climate_change = list(
    path = list.files(pattern = "^climate"),
    hru = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_climate_change/Scenarios/Default/TxtInOut/hru-data.hru",
    con = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_climate_change/Scenarios/Default/TxtInOut/hru.con"
  ),
  LUC = list(
    path = list.files(pattern = "^LUC_\\d"),
    hru =  "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_LUC/Scenarios/Default/TxtInOut/hru-data.hru",
    con =  "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_LUC/Scenarios/Default/TxtInOut/hru.con"
  ),
  LUC_climate = list(
    path = list.files(pattern = "^LUC_climate"),
    hru =  "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_LUC/Scenarios/Default/TxtInOut/hru-data.hru",
    con =  "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_LUC/Scenarios/Default/TxtInOut/hru.con"
  ),
  combined = list(
    path = list.files(pattern = "^combined_"),
    hru = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_combined/Scenarios/Default/TxtInOut/hru-data.hru",
    con = "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0_combined/Scenarios/Default/TxtInOut/hru.con"
  )
)

#### Calculate the Qout per scenario ####
Q_list <- list()
fdc_list <- list()
for (scen in names(scenarios_paths)){
  
  #load scenario
  scenario <- load_swat_run(scenarios_paths[[scen]][["path"]], add_parameter = FALSE, add_run_info = FALSE)
  
  #gather the simulation results
  Q_list[[scen]][["outlet"]] <- scenario$simulation$flo_mon_105
  Q_list[[scen]][["Thondwe"]] <- scenario$simulation$flo_mon_267
  
  #calulate flow duration curves
  fdc_list[[scen]] <- calc_fdc(Q_list[[scen]][["outlet"]])
  
  Q_list[[scen]] <- bind_rows(Q_list[[scen]], .id = "location")

  
}

#bind to a dataframe
Q_df <- bind_rows(Q_list,.id = "scenario")%>%
  filter(location == "outlet")

fdc_df <- bind_rows(fdc_list, .id = "scenario")


#plot flow_out
ggplot(Q_df, aes(date,run_1, color = scenario))+
  geom_line()+
  #scale_x_date(limits = as.Date(c('2022-10-01', '2023-7-01')))+
  ggtitle(paste0("Flow monthly at the outlet in m3/s"))
 ggsave(paste0("./graphs/Q_out_monthly.png"))


#plot fdcs
ggplot(fdc_df,aes(p,run_1, color = scenario))+
  geom_line()+
  ggtitle(paste0("Flow duration curve for monthly flow at the outlet"))
 ggsave(paste0("./graphs/fdc_month.png"))
 

 #### Calculate AQU level WB components
 results <- list()
 for (scen in names(scenarios_paths)) {
   results[[scen]] <- load_swat_run(scenarios_paths[[scen]][['path']])[['simulation']]
 }

scenarios <- names(results)
outputs   <- names(results[[1]])
 
aqu_df <- bind_rows(aqu_res, .id = 'scenario')

#plot aquifer outflow
for (par in unique(aqu_df$param)) {
  aqu_filt <- aqu_df%>%
    filter(param == par)

  ggplot(aqu_filt,aes(date,flow, color = scenario))+
  geom_line()+
  labs(y = par)
  ggtitle(paste0(par, ' in mm'))
  ggsave(paste0("./graphs/aqu_",par,".png"))
}

#### Calculate absolute difference from baseline
baseline_name <- "baseline"  # <-- adjust if needed

baseline_df <- aqu_df %>%
  filter(scenario == baseline_name) %>%
  select(param, date, flow) %>%
  rename(flow_baseline = flow)

aqu_abdiff <- aqu_df %>%
  filter(scenario != baseline_name) %>%
  left_join(baseline_df, by = c("param", "date")) %>%
  mutate(flow_diff = flow - flow_baseline)

aqu_percdiff <- aqu_df %>%
  filter(scenario != baseline_name) %>%
  left_join(baseline_df, by = c("param", "date")) %>%
  mutate(flow_diff = (flow - flow_baseline)/flow_baseline*100)

#### Plot absolute difference per parameter
for (par in unique(aqu_diff$param)) {
  aqu_filt <- aqu_diff %>%
    filter(param == par)
  
  p <- ggplot(aqu_filt, aes(date, flow_diff, color = scenario)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # zero reference line
    labs(
      y = paste0("\u0394 ", par, " (mm)"),                               # delta symbol in label
      title = paste0("Absolute difference from baseline: ", par, " (mm)")
    )
  
  ggsave(paste0("./graphs/aqu_abdiff_", par, ".png"), plot = p)
}

 

#### Caluclate HRU level WB components ####
hru_res <-list()
for (scen in names(scenarios)) {
  hru_res[[scen]] <- HRU_processing(scenarios[[scen]])
}
scenarios
 

#select variables and landuse to plot
hru_vars <- c('SQ_mon','LQ_mon','SW_mon','ET_mon')


#Plotting absolute values of the waterbalance components
for (variable in names(results[[scen]])){

   selected_data <- inner_join(hru_res$baseline[[variable]],hru_res$SWC[[variable]], join_by(date))%>%
    rename(base = ET.x, swc = ET.y)%>%
    inner_join(y = hru_res$climate_change[[variable]], join_by(date))%>%
    rename(climate = ET)%>%
    inner_join(y = hru_res$LUC[[variable]], join_by(date))%>%
    rename(LUC = ET)%>%
    inner_join(y = hru_res$LUC_climate[[variable]], join_by(date))%>%
    rename(LUC_climate = ET)%>%
    inner_join(y = hru_res$combined[[variable]], join_by(date))%>%
    rename(combined = ET)
 
   # hru_res$baseline[[variable]]%>%
   #   rename(base = ET)
   # 

  #mutate(perc_diff = (ET_swc - ET_base) /ET_base*100)

  ggplot(selected_data, aes(x = date))+
   geom_line(aes(y= base, color = 'baseline'))+
   geom_line(aes(y= swc, color = 'swc'))+
   geom_line(aes(y= climate, color = 'climate'))+
   geom_line(aes(y= LUC, color = "LUC"))+
   geom_line(aes(y= LUC_climate, color = 'LUC_climate'))+
   geom_line(aes(y= combined, color = 'combined'))+
   #geom_line(aes(y= perc_diff), color = "green")+
    ggtitle( paste(variable))
   ggsave(paste0("./","_",variable,".png"))

}

#plotting absolute/percentual differences to the baseline scenario

for (variable in hru_vars){
  
  selected_data <- inner_join(hru_res$baseline[[variable]],hru_res$SWC[[variable]], join_by(date))%>%
    rename(base = ET.x, swc = ET.y)%>%
    inner_join(y = hru_res$climate_change[[variable]], join_by(date))%>%
    rename(climate = ET)%>%
    inner_join(y = hru_res$LUC[[variable]], join_by(date))%>%
    rename(LUC = ET)%>%
    inner_join(y = hru_res$LUC_climate[[variable]], join_by(date))%>%
    rename(LUC_climate = ET)%>%
    inner_join(y = hru_res$combined[[variable]], join_by(date))%>%
    rename(combined = ET)
  
  # hru_res$baseline[[variable]]%>%
  #   rename(base = ET)
  # 
  
  #mutate(perc_diff = (ET_swc - ET_base) /ET_base*100)
  
  ggplot(selected_data, aes(x = date))+
    geom_line(aes(y= (swc - base)/base*100, color = 'swc'))+
    geom_line(aes(y= (climate - base)/base*100, color = 'climate'))+
    geom_line(aes(y= (LUC - base)/base*100, color = "LUC"))+
    geom_line(aes(y= (LUC_climate - base)/base*100, color = 'LUC_climate'))+
    geom_line(aes(y= (combined - base)/base*100, color = 'combined'))+
    #geom_line(aes(y= perc_diff), color = "green")+
    labs(title =  paste("Percentual difference to baseline. Land use: Parameter:",variable), y = "percent")
  ggsave(paste0("./graphs/percdiff_",LU,"_",variable,".png"))
  
}



Q_hist <- read.csv("C:/Users/31638/Documents/Studie/Msc_Thesis/Data/Malawi_climate_hydro/Thondwe_runoff.csv")
Q_hist$Date <- as.Date(Q_hist$Date, "%d/%m/%Y")

ggplot(Q_hist,aes(Date,Discharge..cumecs.))+
  geom_line()+
  scale_x_date(limits = as.Date(c('1980-10-01', '2010-7-01')),date_breaks = "1 year")+
  ylim(0,10)+
  ggtitle("Historical Discharge at Thondwe")
ggsave(paste0("./graphs/Historical_Q.png"))
