
Scenario_processing <- function(scenarios, hru_file, con_file){
  final_result <- list()
  for (scen in names(scenarios)) {
  
    #Load the part of the list containing the simulations and create an empty list
    simulation <- scenarios[[scen]]$simulation
    outputs <- c("ET_mon","SQ_mon","LQ_mon", "SW_mon","PR_mon")
    result_list <- list()
    
    #Loop through each HRU result and store these in a list
    for (out in outputs) {
      tmp_list <- list()
      sim_select <- grep(paste0('^',out), names(simulation), value = TRUE)
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
  
      #Calculate the area average ET per landuse type
      result_list[[out]] <- result_list[[out]] %>%
        group_by(lu_mgt,date) %>%
        summarise(ET = sum(ET*area)/sum(area),
                  across(all_of(grep("^run_\\d+$", names(result_list[[out]]), value = TRUE)),
                         ~ sum(.x * area) / sum(area),
                         .names = "{.col}"),
                  .groups = 'drop')%>%
        mutate(date = as.Date(date))
    }
    final_result[[scen]] <- result_list
  }
  return(final_result)
}
