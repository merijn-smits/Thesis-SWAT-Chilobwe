library(lubridate)

dir <- "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0/Scenarios/Default/Scenarios/Default/TxtInOut/"

con <- read.delim(paste0(dir,"aquifer.con"), sep = "", skip = 1, header= FALSE, fill = TRUE)[c(3,4)][-1,]%>%
  mutate(V3 = as.numeric(V3),V4 = as.numeric(V4))
colnames(con) <- c("gis_id", "area")

aqu <- read.delim(paste0(dir,"aquifer_mon.txt"), sep = "", skip = 1)[-1,]%>%
  select(gis_id,mon,yr,
         flo,revap,dep_wt,seep,rchrg)%>%
  mutate(date = as.Date(make_date(year = yr,month = mon, day = 1), "%Y-%m-%d"))%>%
  select(!c(mon,yr))%>%
  mutate(flo = as.numeric(flo),
         revap = as.numeric(revap), 
         dep_wt= as.numeric(dep_wt), 
         seep = as.numeric(seep),
         rchrg = as.numeric(rchrg),
         gis_id = as.numeric(gis_id)
         )
  
aqu <- inner_join(x = aqu, y = con, by = 'gis_id')%>%
  filter(gis_id != 14)
aqu_list <- list()

for (flow in colnames(aqu[c(2:6)])) {
  aqu_list[[flow]] <-aqu%>%
    group_by(date) %>%
    summarise(value = sum(.data[[flow]]*area)/sum(area),
              .groups = 'drop')%>%
    mutate(date = as.Date(date))
}
aqu_tot <- bind_rows(aqu_list,.id = "variable")


  
ggplot(aqu_tot, aes(x = date, y = value, color = variable))+
  geom_line()+
  ggtitle("Aquifer compenents for no calibration")
         
