
AQU_processing <- function(scenario_paths){
  
  #load the scenario from file
  scenario <- load_swat_run(scenario_paths[["path"]], add_parameter = FALSE)
  
  #gather the simulation results
  simulation <- scenario$simulation
  
  #load the .con file

  aqu_con_file <- read.table(paste0(scenario_paths[["con"]],'/../aquifer.con'), skip = 2, header = FALSE, fill = TRUE)[,c(1,4)]
  
  colnames(aqu_con_file) <- c('id', 'area')
  
  #define outputs to load
  outputs <- c("AQ_mon","WT_mon","RE_mon", "RC_mon")
  result_list <- list()
    
    #Loop through each output type
    for (out in outputs) {
      tmp_list <- list()
      #select one output type and remove the deep aquifer (in this case 42)
      sim_select <- grep(paste0('^',out), names(simulation), value = TRUE)[-grep("42$", names(simulation))]
      print(paste(out,"selected"))
      
    #loop through the aqus
     for (aqu_name in sim_select) {
      #get the aqu number from the long name
      aqu_id   <- as.integer(sub(paste0(out,'_'),'',aqu_name))
      
      # load the data for the aqu and add a column with 
      aqu_sim <- simulation[[aqu_name]] %>%
        mutate(AQU = aqu_id) %>%
        relocate(AQU,date) %>%
        rename(flow = run_1)
      
      tmp_list[[length(tmp_list)+1]] <- aqu_sim
     }
      result_list[[out]] <- bind_rows(tmp_list)
      
      # join with con file data
      result_list[[out]] <- inner_join(result_list[[out]],aqu_con_file, by = c('AQU' = 'id'))
        

        #Calculate the area average 
        result_list[[out]] <- result_list[[out]] %>%
         filter('id' != 42)%>%
          group_by(date) %>%
          summarise(flow = sum(flow*area)/sum(area),
                    across(all_of(grep("^run_\\d+$", names(result_list[[out]]), value = TRUE)),
                           ~ sum(.x * area) / sum(area),
                           .names = "{.col}"),
                    .groups = 'drop')%>%
          mutate(date = as.Date(date))
        print("area average calculated")
      
    }
  return(result_list)
}

