rm(list = ls())
# This script will tidy flow data


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(gghighlight)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(dbhydroR)




# Import Data --------------------------------------------------------------

#Inflow Stations
G378D_C_BK <- get_hydro(dbkey = "64484", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378C_C_BK <- get_hydro(dbkey = "64483", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378B_C_BK <- get_hydro(dbkey = "64482", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378A_C_BK <- get_hydro(dbkey = "64481", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34

#Outflow Stations
G379D_C_BK <- get_hydro(dbkey = "64489", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
G379C_C_BK <- get_hydro(dbkey = "64488", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
G379B_C_BK <- get_hydro(dbkey = "64487", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
G379A_C_BK <- get_hydro(dbkey = "64486", date_min="2021-06-01",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34



# Tidy Data ---------------------------------------------------------------

#Tidy Inflow
G378_Flow <- setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(2022,05,31,0,0,0,tz = "US/Eastern"),by = "1 min")),"date") %>%
bind_rows(G378D_C_BK) %>%
bind_rows(G378C_C_BK) %>%
bind_rows(G378B_C_BK) %>%
bind_rows(G378A_C_BK) %>%
arrange(date) %>%
fill(G378D_C_FLOW_cfs,G378C_C_FLOW_cfs,G378B_C_FLOW_cfs,G378A_C_FLOW_cfs) %>%
mutate(G378_Flow=rowSums(.[2:5],na.rm=TRUE)) %>%
group_by(date) %>%
summarise(G378_inflow=sum(G378_Flow,na.rm=TRUE))
       
#Tidy Inflow
G379_Flow <- setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(2022,05,31,0,0,0,tz = "US/Eastern"),by = "1 min")),"date") %>%
bind_rows(G379D_C_BK) %>%
bind_rows(G379C_C_BK) %>%
bind_rows(G379B_C_BK) %>%
bind_rows(G379A_C_BK) %>%
arrange(date) %>%
fill(G379D_C_FLOW_cfs,G379C_C_FLOW_cfs,G379B_C_FLOW_cfs,G379A_C_FLOW_cfs) %>%
mutate(G379_Flow=rowSums(.[2:5],na.rm=TRUE)) %>%
group_by(date) %>%
summarise(G379_outflow=sum(G379_Flow,na.rm=TRUE))

#Flow 30 min frequency
Flow_Data <- left_join(G378_Flow,G379_Flow,by="date") %>%
mutate(period=year(date)*365*24*60+yday(date)*24*60+hour(date)*60+minute(date)) %>%   #there are double the amount of periods for daylight savings on 11/7/21- Issue unresolved
mutate(period_30=floor(period/30)) %>%
group_by(period_30) %>%
mutate(`Mean inflow (cfs)`=mean(G378_inflow,na.rm=TRUE),`Mean outflow (cfs)`=mean(G379_outflow,na.rm=TRUE))  %>%
filter(minute(date)%%30 == 0)   %>%
filter(date < today()) %>%  
ungroup() %>%  
rename(`Date Time`="date") %>%
select(`Date Time`,`Mean inflow (cfs)`,`Mean outflow (cfs)`)

#Flow 1 min frequency
Flow_Data_1_min <- left_join(G378_Flow,G379_Flow,by="date") %>%
filter(date < today()) %>%  
rename(`Date Time`="date",`Inflow (cfs)`="G378_inflow",`Outflow (cfs)`="G379_outflow") 



# Save Data ---------------------------------------------------------------

write.csv(Flow_Data_1_min ,"./Data/Flow Data/Flow_Data_1_min.csv",row.names = FALSE)
write.csv(Flow_Data ,"./Data/Flow Data/Flow.csv",row.names = FALSE)


