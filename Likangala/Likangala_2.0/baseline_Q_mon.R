library(lubridate)

dir <- "C:/Users/31638/Documents/Studie/Msc_Thesis/SWAT/Likangala/Likangala_2.0/Likangala_2.0/Scenarios/Default/Scenarios/Default/TxtInOut/"

flo <- read.delim(paste0(dir,"channel_sd_mon.txt"),sep = "",skip = 1)[-1]%>%
  filter(gis_id == 267 | gis_id == 105)%>%
  select(gis_id,mon,yr,
         flo_out)%>%
  mutate(date = as.Date(make_date(year = yr,month = mon, day = 1), "%Y-%m-%d"))%>%
  select(!c(mon,yr))%>%
  mutate(flo_out = as.numeric(flo_out))

ggplot(flo,aes(date, flo_out, color = gis_id))+
  geom_line()+
  ggtitle("modelled discharge at outlet in m3/s without calibration")
ggsave("Qout_no_cal.png")
 