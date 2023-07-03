rm(list = ls())
# This script will tidy flow data


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(Hmisc)
library(zoo)
library(dbhydroR)


# Import Data --------------------------------------------------------------

#Inflow Stations STA-34
G378D_C_BK <- get_hydro(dbkey = "64484", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378C_C_BK <- get_hydro(dbkey = "64483", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378B_C_BK <- get_hydro(dbkey = "64482", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378A_C_BK <- get_hydro(dbkey = "64481", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34

#Outflow Stations STA-3/4
G379D_C_BK <- get_hydro(dbkey = "64489", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 2B of STA34
G379C_C_BK <- get_hydro(dbkey = "64488", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 2B of STA34
G379B_C_BK <- get_hydro(dbkey = "64487", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 2B of STA34
G379A_C_BK <- get_hydro(dbkey = "64486", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 2B of STA34

#Inflow Stations STA-3/4
G378D_C_BK <- get_hydro(dbkey = "64484", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378C_C_BK <- get_hydro(dbkey = "64483", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378B_C_BK <- get_hydro(dbkey = "64482", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34
G378A_C_BK <- get_hydro(dbkey = "64481", date_min="2021-06-01",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 2B of STA34

#Outflow Stations STA-1W
G306A_C_BK <- get_hydro(dbkey = "64290", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306B_C_BK <- get_hydro(dbkey = "64291", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306C_C_BK <- get_hydro(dbkey = "64296", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306D_C_BK <- get_hydro(dbkey = "64297", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306E_C_BK <- get_hydro(dbkey = "64299", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306F_C_BK <- get_hydro(dbkey = "64300", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306G_C_BK <- get_hydro(dbkey = "64301", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306H_C_BK <- get_hydro(dbkey = "64302", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306I_C_BK <- get_hydro(dbkey = "64303", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W
G306J_C_BK <- get_hydro(dbkey = "64304", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for Outflow to Cell 5B of STA1W

#Inflow Stations STA-1W
G304A_C_BK <- get_hydro(dbkey = "64277", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304B_C_BK <- get_hydro(dbkey = "64278", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304C_C_BK <- get_hydro(dbkey = "64279", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304D_C_BK <- get_hydro(dbkey = "64280", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304E_C_BK <- get_hydro(dbkey = "64281", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304F_C_BK <- get_hydro(dbkey = "64282", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304G_C_BK <- get_hydro(dbkey = "64283", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304H_C_BK <- get_hydro(dbkey = "64284", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304I_C_BK <- get_hydro(dbkey = "64285", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W
G304J_C_BK <- get_hydro(dbkey = "64286", date_min="2022-10-18",date_max=as.character(Sys.Date()))  #DBHYDRO data for inflow to Cell 5B of STA1W

# Tidy Data ---------------------------------------------------------------

#Tidy Inflow STA-34
G378_Flow <- setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(year(today()),month(today()),day(today()),0,0,0,tz = "US/Eastern"),by = "1 min")),"date") %>%
left_join(G378D_C_BK,by="date") %>%
left_join(G378C_C_BK) %>%
left_join(G378B_C_BK) %>%
left_join(G378A_C_BK) %>%
arrange(date) %>%
fill(G378D_C_FLOW_cfs,G378C_C_FLOW_cfs,G378B_C_FLOW_cfs,G378A_C_FLOW_cfs) %>%
mutate(G378_Flow=rowSums(.[2:5],na.rm=TRUE)) %>%
group_by(date) %>%
mutate(G378_inflow=mean(G378_Flow,na.rm=TRUE)) %>%
distinct(date,G378_inflow)  
       
#Tidy Outflow STA-34
G379_Flow <- setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(year(today()),month(today()),day(today()),0,0,0,tz = "US/Eastern"),by = "1 min")),"date") %>%
left_join(G379D_C_BK) %>%
left_join(G379C_C_BK) %>%
left_join(G379B_C_BK) %>%
left_join(G379A_C_BK) %>%
arrange(date) %>%
fill(G379D_C_FLOW_cfs,G379C_C_FLOW_cfs,G379B_C_FLOW_cfs,G379A_C_FLOW_cfs) %>%
mutate(G379_Flow=rowSums(.[2:5],na.rm=TRUE)) %>%
group_by(date) %>%
mutate(G379_outflow=mean(G379_Flow,na.rm=TRUE)) %>%
distinct(date,G379_outflow)  

#Tidy Outflow STA-1W
G306_Flow <- setNames(as.data.frame(seq(from=ISOdate(2022,10,18,0,0,0,tz = "US/Eastern"), to=ISOdate(year(today()),month(today()),day(today()),0,0,0,tz = "US/Eastern"),by = "1 min")),"date") %>%
left_join(G306A_C_BK) %>%
left_join(G306B_C_BK) %>%
left_join(G306C_C_BK) %>%
left_join(G306D_C_BK) %>%
left_join(G306E_C_BK) %>%
left_join(G306F_C_BK) %>%
left_join(G306G_C_BK) %>%
left_join(G306H_C_BK) %>%  
left_join(G306I_C_BK) %>%
left_join(G306J_C_BK) %>%  
arrange(date) %>%
fill(G306A_C_FLOW_cfs,G306B_C_FLOW_cfs,G306C_C_FLOW_cfs,G306D_C_FLOW_cfs,G306E_C_FLOW_cfs,G306F_C_FLOW_cfs,G306G_C_FLOW_cfs,G306H_C_FLOW_cfs,G306I_C_FLOW_cfs,G306J_C_FLOW_cfs) %>%
mutate(G306_Flow=rowSums(.[2:11],na.rm=TRUE)) %>%
group_by(date) %>%
mutate(G306_outflow=mean(G306_Flow,na.rm=TRUE)) %>%
distinct(date,G306_outflow)  

#Tidy Inflow STA-1W
G304_Flow <- setNames(as.data.frame(seq(from=ISOdate(2022,10,18,0,0,0,tz = "US/Eastern"), to=ISOdate(year(today()),month(today()),day(today()),0,0,0,tz = "US/Eastern"),by = "1 min")),"date") %>%
left_join(G304A_C_BK) %>%
left_join(G304B_C_BK) %>%
left_join(G304C_C_BK) %>%
left_join(G304D_C_BK) %>%
left_join(G304E_C_BK) %>%
left_join(G304F_C_BK) %>%
left_join(G304G_C_BK) %>%
left_join(G304H_C_BK) %>%  
left_join(G304I_C_BK) %>%
left_join(G304J_C_BK) %>%  
arrange(date) %>%
fill(G304A_C_FLOW_cfs,G304B_C_FLOW_cfs,G304C_C_FLOW_cfs,G304D_C_FLOW_cfs,G304E_C_FLOW_cfs,G304F_C_FLOW_cfs,G304G_C_FLOW_cfs,G304H_C_FLOW_cfs,G304I_C_FLOW_cfs,G304J_C_FLOW_cfs) %>%
mutate(G304_Flow=rowSums(.[2:11],na.rm=TRUE)) %>%
group_by(date) %>%
mutate(G304_inflow=mean(G304_Flow,na.rm=TRUE)) %>%
distinct(date,G304_inflow)  


#Flow 30 min frequency STA-34
Flow_Data <- left_join(G378_Flow,G379_Flow,by="date") %>%
ungroup() %>%  
mutate(period=year(date)*365*24*60+yday(date)*24*60+hour(date)*60+minute(date)) %>%   #there are double the amount of periods for daylight savings on 11/7/21- Issue unresolved
mutate(period_30=floor(period/30)) %>%
group_by(period_30) %>%
mutate(`Mean inflow (cfs)`=mean(G378_inflow,na.rm=TRUE),`Mean outflow (cfs)`=mean(G379_outflow,na.rm=TRUE))  %>%
filter(minute(date)%%30 == 0)   %>%
filter(date < today()) %>%  
ungroup() %>%  
rename(`Date Time`="date") %>%
select(`Date Time`,`Mean inflow (cfs)`,`Mean outflow (cfs)`) %>% 
mutate(`Mean inflow (cfs)`=if_else(`Mean inflow (cfs)`<0,0,`Mean inflow (cfs)`))  #Midflow data accuracy is questionable. All negative values changed to 0. 

#Flow 1 min frequency STA-34
Flow_Data_1_min <- left_join(G378_Flow,G379_Flow,by="date") %>%
ungroup() %>%  
filter(date < today()) %>%  
rename(`Date Time`="date",`Inflow (cfs)`="G378_inflow",`Outflow (cfs)`="G379_outflow") %>%
mutate(`Inflow (cfs)`=if_else(`Inflow (cfs)`<0,0,`Inflow (cfs)`))  #Midflow data accuracy is questionable. All negative values changed to 0. 

#Flow 1 min frequency STA-1W
Flow_Data_1_min_STA_1W <- left_join(G304_Flow,G306_Flow,by="date") %>%
ungroup() %>%  
filter(date < today()) %>%  
rename(`Date Time`="date",`Inflow (cfs)`="G304_inflow",`Outflow (cfs)`="G306_outflow") # %>%
#mutate(`Inflow (cfs)`=if_else(`Inflow (cfs)`<0,0,`Inflow (cfs)`))  #Midflow data accuracy is questionable. All negative values changed to 0. 

#Flow 30 min frequency STA-1W
Flow_Data_STA_1W <- left_join(G304_Flow,G306_Flow,by="date") %>%
ungroup() %>%  
mutate(period=year(date)*365*24*60+yday(date)*24*60+hour(date)*60+minute(date)) %>%   #there are double the amount of periods for daylight savings on 11/7/21- Issue unresolved
mutate(period_30=floor(period/30)) %>%
group_by(period_30) %>%
mutate(`Mean inflow (cfs)`=mean(G304_inflow,na.rm=TRUE),`Mean outflow (cfs)`=mean(G306_outflow,na.rm=TRUE))  %>%
filter(minute(date)%%30 == 0)   %>%
filter(date < today()) %>%  
ungroup() %>%  
rename(`Date Time`="date") %>%
select(`Date Time`,`Mean inflow (cfs)`,`Mean outflow (cfs)`) %>% 
mutate(`Mean inflow (cfs)`=if_else(`Mean inflow (cfs)`<0,0,`Mean inflow (cfs)`))  #Midflow data accuracy is questionable. All negative values changed to 0. 



# Save Data ---------------------------------------------------------------


write.csv(Flow_Data_1_min ,"./Data/Flow Data/Flow_Data_1_min.csv",row.names = FALSE) #STA-34 1 min frequency
write.csv(Flow_Data ,"./Data/Flow Data/Flow.csv",row.names = FALSE) #STA34 30 minute frequency
write.csv(Flow_Data_STA_1W  ,"./Data/Flow Data/Flow_STA_1W.csv",row.names = FALSE) #STA1W 30 min frequency 
write.csv(Flow_Data_1_min_STA_1W ,"./Data/Flow Data/Flow_Data_1_min_STA_1W.csv",row.names = FALSE) #STA-1W

