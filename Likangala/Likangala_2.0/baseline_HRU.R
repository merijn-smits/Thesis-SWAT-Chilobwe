library(lubridate)

dir <- "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0/Scenarios/Default/TxtInOut/"
obs_file <- "C:/Users/31638/Documents/Studie/Msc_Thesis/Calibration_data/Likangala_WAPOR/Result/Basin.csv"

obs <- read.csv(obs_file)%>%
  pivot_longer(c(2:87))%>%
  mutate(date = as.Date(paste0(substr(name,2,8),'.01'),"%Y.%m.%d"),
         variable = 'obs',
         name = NULL,
         Subbasin = NULL)

con <- read.delim(paste0(dir,"hru.con"), sep = "", skip = 1, header= FALSE, fill = TRUE)[c(3,4)][-1,]%>%
  mutate(V3 = as.numeric(V3),V4 = as.numeric(V4))
colnames(con) <- c("gis_id", "area")

hru <- read.delim(paste0(dir,"hru_wb_mon.txt"), sep = "", skip = 1)[-1,]%>%
  select(gis_id,mon,yr,
         et, surq_gen, latq, perc)%>%
  mutate(date = as.Date(make_date(year = yr,month = mon, day = 1), "%Y-%m-%d"))%>%
  select(!c(mon,yr))%>%
  mutate(et = as.numeric(et),
         surq_gen = as.numeric(surq_gen), 
         latq = as.numeric(latq),
         #sw_final = as.numeric(sw_final),
         perc = as.numeric(perc),
         gis_id = as.numeric(gis_id)
         )
  

hru <- inner_join(x = hru, y = con, by = 'gis_id')%>%
  filter(gis_id != 14)
hru_list <- list()

for (flow in colnames(hru[c(2:5)])) {
  hru_list[[flow]] <-hru%>%
    group_by(date) %>%
    summarise(value = sum(.data[[flow]]*area)/sum(area),
              .groups = 'drop')%>%
    mutate(date = as.Date(date))
}
hru_tot <- bind_rows(hru_list,.id = "variable")
hru_bind <- rbind(hru_tot,obs)

  
ggplot(hru_bind, aes(x = date, y = value, color = variable))+
  geom_line()+
  ggtitle("HRU waterbalance components")
         


