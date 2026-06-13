#Analysis script to compare the different results
library(tidyverse)
library(SWATtunR)
library(SWATrunR)
source('Scenario_processing.r')
source('AQU_processing.r')

#file locations
base_path <- "./Chilobwe_calibrated/baseline_cal"
current_swc_path <- "./Chilobwe_SWC/current_swc_cal" 
hru_path <- "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Chilobwe_SWC/Scenarios/Default/TxtInOut/hru-data.hru"
con_path <- "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Chilobwe_SWC/Scenarios/Default/TxtInOut/hru.con"
aqu_con <- "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Chilobwe_SWC/Scenarios/Default/TxtInOut/aquifer.con"
  
scenario_paths  <-list(base = base_path,
                       swc  = current_swc_path)
#load runs
baseline <- load_swat_run(base_path)
current_swc <- load_swat_run(current_swc_path)


scenarios = list(baseline    = baseline, 
                 current_swc = current_swc)

### Q at outlet ###

for (i in cha_ids) {
  
fdc_list <- list()
Q_list <- list()
for (scen in names(scenarios)) {
 
  
  #gather the simulation results
  Q_list[[scen]] <- scenarios[[scen]][["simulation"]][[paste0("flo_day_",i)]]

  
  #calulate flow duration curves
  fdc_list[[scen]] <- calc_fdc(Q_list[[scen]])
  
  
}

# Q_df <- bind_rows(Q_list,  .id = "Scenario")
# ggplot(Q_df, aes(date, run_1, color = Scenario))+
#   geom_line()
# ggsave(paste0("./graphs/Q_cha_day",i,".png"))

# fdc_df <- bind_rows(fdc_list, .id = "Scenario")
# ggplot(fdc_df,aes(p,run_1, color = Scenario))+
#   geom_line()
# ggsave(paste0("./graphs/temp/fdc_cha_",i,".png"))

difference <- inner_join(Q_list$baseline, Q_list$current_swc, by = "date" , suffix = names(Q_list))%>%
  mutate(diff =  run_1current_swc - run_1baseline,
         pdiff = ifelse(run_1baseline> 0.001, (run_1current_swc - run_1baseline)/run_1baseline*100,0)
  )

ggplot(difference,aes(x = date, y = diff))+
  geom_line()+
  labs(title = paste0("Absolute difference in discharge between SWC and Baseline scenarios for channel ",i), y = "Q (m3/s)")
ggsave(paste0("./graphs/temp/Diff_cha_day",i,".png"))
}
         

  ### AQU level components ###
aqu_res <- list()
for (scen in names(scenario_paths)) {
  aqu_res[[scen]] <- AQU_processing(scenario_paths[[scen]], aqu_con)
  aqu_res[[scen]] <- bind_rows(aqu_res[[scen]], .id = 'param')
}
aqu_df <- bind_rows(aqu_res, .id = 'scenario')

#plot aquifer outflow
for (par in unique(aqu_df$param)) {
  aqu_filt <- aqu_df%>%
    filter(param == par)
  
  ggplot(aqu_filt,aes(date,flow, color = scenario))+
    geom_line()+
    ggtitle(paste0(par, ' in mm'))
  ggsave(paste0("./graphs/aqu_",par,".png"), scale = 2)
}



### HRU level components###

# merge results per landuse and plot in one graph
hru_file <- read.table(hru_path, skip = 1, header = TRUE)[,c(1,6)]%>%
  mutate(lu_mgt = substr(lu_mgt,1,4))
con_file <- read.table(con_path, skip = 1, header = TRUE)[,c(1,4)]

result <- Scenario_processing(scenarios, hru_file, con_file)


#select variables and landuse to plot
LU <- 'agdt'
variable <- 'PR_mon'

selected_data <- inner_join(result$baseline[[variable]],result$current_swc[[variable]], join_by(lu_mgt,date))%>%
  filter(lu_mgt == LU)%>%
  rename(ET_base = ET.x, ET_swc = ET.y)%>%
  mutate(perc_diff = ifelse(ET_swc>1,
                            (ET_swc - ET_base) /ET_base*100,
                            0))
scale = 1

ggplot(selected_data, aes(x = date))+
  #geom_line(aes(y= ET_base, color = "ET_base" ))+
  #geom_line(aes(y= ET_swc, color = 'ET_SWC'))+
  #geom_line(aes(y= ET_climate), color = 'ET_climate')+
  geom_line(aes(y= perc_diff/scale), color = "darkgreen")+
  #scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "percent difference"))+
  labs(title = paste0("Percentual change in ", variable," by applying SWC"), y = "Percent" )
 ggsave(paste0("./graphs/",LU,"_percent_",variable,".png"))



