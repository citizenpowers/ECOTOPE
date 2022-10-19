#This script joins all sensor data and WQ data into a single data frame for analysis. 

rm(list = ls())

#Steps
#1.) Upload Light, Depth, SOnde, and WQ Data from .csv files. If updates are needed run those scripts to update.  
#2.)
#3.) 

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

# Import data -------------------------------------------------------------


Water_Depth_Data <- read_csv("Data/Levelogger/Water_Depth_Data.csv")
All_light_data <- read_csv("./Data/HOBO/All_light_data.csv")
All_Sonde_long <- read.csv("Data/Sonde/All_Sonde_long.csv",encoding = "Latin-1",check.names=FALSE)
WQ_Data_Tidy <- read_csv("Data/WQ Data/WQ_Data_Tidy.csv")  #all WQ data 
Field_data <- read_csv("Data/Field Data/Field_data.csv")
Flow_Data <- read_csv("Data/Flow Data/Flow.csv")
Wind_data <- read_csv( "./Data/Weather Data/Wind_data.csv")


# WQ and Field data---------------------------------------------------------------

WQ_Field_Data <- WQ_Data_Tidy %>% 
filter(MATRIX=="SW",COLLECT_METHOD %in% c("G","GP"),) %>%
mutate(Hour=hour(COLLECT_DATE),Minute=minute(COLLECT_DATE)) %>%
select(Date,Ecotope,Position,TEST_NAME,VALUE,Hour,Minute)  %>%
mutate(Minute=case_when(between(Minute,15,44)~30,!between(Minute,15,44)~0))  %>%    #round time to nearest 30 minutes so it can be joined with continuous sensor data
bind_rows(Field_data) %>%
pivot_wider(names_from =c(TEST_NAME),values_from=VALUE,values_fn = mean) #Used values_fn = mean to average duplicate values

# Create DF of differences between upstream and downstream -----------------

WQ_Upstream <- WQ_Field_Data  %>%
filter(Position=="Upstream") %>%
pivot_longer(names_to = "TEST_NAME",values_to="VALUE",6:33)  %>%
mutate(`Upstream Values`=VALUE)  %>%
select(Date,Ecotope,TEST_NAME,`Upstream Values`)

WQ_DownStream <- WQ_Field_Data  %>%
filter(Position=="Downstream") %>%
pivot_longer(names_to = "TEST_NAME",values_to="VALUE",6:33)  %>%
mutate(`Downstream Values`=VALUE) %>%
select(Date,Ecotope,TEST_NAME,`Downstream Values`) 

WQ_Upstream_Downstream_Tidy <- WQ_Upstream %>%
left_join(WQ_DownStream,by=c("Date","Ecotope","TEST_NAME"))  %>%
mutate(`Difference`=`Upstream Values`-`Downstream Values`)

# Join differences to WQ and field data -----------------------------------

WQ_Field_Diff_Data <- WQ_Field_Data %>%
left_join(pivot_wider(select(WQ_Upstream_Downstream_Tidy,Date,Ecotope,TEST_NAME,Difference),names_from =c(TEST_NAME),values_from=Difference,names_prefix = "Dif ",values_fn = mean) ,by=c("Date","Ecotope"))
# Join continuous data -------------------------------

Continuous_data <-  setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(year(today()),month(today()),day(today()),0,0,0,tz = "US/Eastern"),by = "30 min")),"Date Time") %>%
left_join(select(pivot_wider(Water_Depth_Data,names_from=Site,values_from=level,values_fn = mean),1:6),by="Date Time") %>%  #join water depth data
mutate(across(where(is.numeric), ~if_else(is.na(.)==F,true = round(.,digits=2),  false = as.numeric(.))))  %>%               #round water depth data do 2 decimal points
pivot_longer(names_to = "Ecotope",values_to = "DCS Levelogger", 2:6)  %>%     
mutate(Ecotope=ifelse(Ecotope=="Southern Naiad","Naiad",Ecotope))  %>%
#mutate(Ecotope=case_when(Ecotope=="Naiad"~"Mixed?",Ecotope=="Mixed"~"Naiad?",TRUE~Ecotope)) %>% #were levelogger sensors switched at mixed and naiad  
#mutate(Ecotope=case_when(Ecotope=="Naiad?"~"Naiad",Ecotope=="Mixed?"~"Mixed",TRUE~Ecotope)) %>% #were levelogger sensors switched at mixed and naiad  
left_join(pivot_wider(All_light_data,names_from=Position,values_from=`Light Intensity Lux`,names_prefix="Light Intensity "),by=c("Date Time","Ecotope")) %>%  #join light data
left_join(mutate(pivot_wider(as.data.frame(All_Sonde_long),names_from=Parameter,values_from=Value,values_fn = mean),`Date Time`=ymd_hms(`Date Time`)),by=c("Date Time","Ecotope")) %>%  #join sonde data
left_join(Flow_Data,by="Date Time") %>%  #join Flow
left_join(Wind_data,by="Date Time")  #join wind data

# Join Continuous Data to WQ and Field Data (continuous and wq data not on same rows)-------------------------------

WQ_Field_Data_Continuous_data <- WQ_Field_Diff_Data %>%
pivot_longer(names_to = "TEST_NAME",values_to="VALUE",6:61)  %>%
mutate(`Date Time`=ISOdate(year(Date),month(Date),day(Date),Hour,Minute,0,tz = "US/Eastern")) %>%
select(-Date,-Hour,-Minute) %>%
bind_rows(mutate(pivot_longer(Continuous_data,names_to = "TEST_NAME",values_to="VALUE",3:22),Position="Mid")) %>%  #Join Continuous data to WQ and field data
pivot_wider(names_from =c(TEST_NAME),values_from=VALUE,values_fn=mean)  

colnames(WQ_Field_Data_Continuous_data) <-enc2utf8(colnames(WQ_Field_Data_Continuous_data))  


# Join continuous data to WQ and Field Data (Continuous and wq on same rows) -----------------------
WQ_Field_with_continuous_same_rows <- WQ_Field_Diff_Data %>%
left_join(select(mutate(Continuous_data,Date=as.Date(`Date Time`),Hour=hour(`Date Time`),Minute=minute(`Date Time`)),-`Date Time`),by=c("Date","Ecotope","Hour","Minute"))  
select(mutate(Continuous_data,Date=as.Date(`Date Time`),Hour=hour(`Date Time`),Minute=minute(`Date Time`)),-`Date Time`)

# Save Data ---------------------------------------------------------------
write.csv(WQ_Upstream_Downstream_Tidy, "./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv",row.names = FALSE)
write.csv(WQ_Field_Data_Continuous_data,"./Data/Joined Data/WQ_Field_Data_Continuous_data.csv",row.names = FALSE)
write.csv(Continuous_data,"./Data/Joined Data/Continuous_data.csv",row.names = FALSE)
write.csv(WQ_Field_Data,"./Data/Joined Data/WQ_Field_Data.csv",row.names = FALSE)
write.csv(WQ_Field_with_continuous_same_rows,"./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",row.names = FALSE)


 