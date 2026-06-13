
HRU_processing <- function(scenario_paths){
  
  #load the scenario from file
  scenario <- load_swat_run(scenario_paths[["path"]], add_parameter = FALSE)
  
  #gather the simulation results
  simulation <- scenario$simulation
  
  #load the .hru and .con file
  hru_file <- read.table(scenario_paths[["hru"]], skip = 1, header = TRUE)[,c(1,6)]%>%
    mutate(lu_mgt = substr(lu_mgt,1,4))
  con_file <- read.table(scenario_paths[["con"]], skip = 1, header = TRUE)[,c(1,4)]
  
  #define outputs to load
  outputs <- c("ET_mon", "SQ_mon","LQ_mon", "SW_mon", "YD_yr")
  result_list <- list()
    
    #Loop through each output type
    for (out in outputs) {
      tmp_list <- list()
      #select one output type
      sim_select <- grep(paste0('^',out), names(simulation), value = TRUE)
      print(paste(out,"selected"))
      
    #loop through the hrus
     for (hru_name in sim_select) {
      #get the hru number from the long name
      hru_id   <- as.integer(sub(paste0(out,'_'),'',hru_name))
      
      # load the data for the hru and add a column with 
      hru_sim <- simulation[[hru_name]] %>%
        mutate(HRU = hru_id) %>%
        relocate(HRU,date) %>%
        rename(ET = run_1)
      
      tmp_list[[length(tmp_list)+1]] <- hru_sim
     }
      result_list[[out]] <- bind_rows(tmp_list)
      
      # join with landuse data
      result_list[[out]] <- inner_join(result_list[[out]],hru_file, by = c('HRU' = 'id'))%>%
        inner_join(con_file, by = c('HRU' = 'id'))
  
      #Calculate the area average ET 
      result_list[[out]] <- result_list[[out]] %>%
        group_by(date) %>%
        summarise(ET = sum(ET*area)/sum(area),
                  .groups = 'drop')%>%
        mutate(date = as.Date(date))
      print("area average calculated")
    }
  return(result_list)
}

