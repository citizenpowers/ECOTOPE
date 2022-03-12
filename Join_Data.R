#This script joins all sensor data and WQ data into a single data frame for analysis. 

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
WQ_Upstream_Downstream_Tidy <- read_csv("Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv") # Water quality with differences between upstream and downstream calculated
WQ_Data_Tidy <- read_csv("Data/WQ Data/WQ_Data_Tidy.csv")  #all WQ data 
Field_data <- read_csv("Data/Field Data/Field_data.csv")


# WQ and Field data---------------------------------------------------------------

WQ_Field_Data <- WQ_Data_Tidy %>% 
filter(MATRIX=="SW",COLLECT_METHOD=="G") %>%
mutate(Hour=hour(COLLECT_DATE),Minute=minute(COLLECT_DATE)) %>%
select(Date,Ecotope,Position,TEST_NAME,VALUE,Hour,Minute)  %>%
mutate(Minute=case_when(between(Minute,15,44)~30,!between(Minute,15,44)~0))  %>%    #round time to nearest 30 minutes so it cna be joined with continuous sensor data
bind_rows(Field_data) %>%
pivot_wider(names_from =c(TEST_NAME),values_from=VALUE,values_fn = mean) #Used values_fn = mean to average duplicate values

# Join continuous data -------------------------------

Continuous_data <-  setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(2022,05,31,0,0,0,tz = "US/Eastern"),by = "30 min")),"Date Time") %>%
left_join(select(pivot_wider(Water_Depth_Data,names_from=Site,values_from=level,values_fn = mean),1:6),by="Date Time") %>%  #join water depth data
mutate(across(where(is.numeric), ~if_else(is.na(.)==F,true = round(.,digits=2),  false = as.numeric(.))))  %>%               #round water depth data do 2 decimal points
pivot_longer(names_to = "Ecotope",values_to = "DCS Levelogger", 2:6)  %>%                                                       
mutate(Ecotope=ifelse(Ecotope=="Southern Naiad","Naiad",Ecotope))  %>%
left_join(pivot_wider(All_light_data,names_from=Position,values_from=`Light Intensity Lux`,names_prefix="Light Intensity "),by=c("Date Time","Ecotope")) %>%  #join light data
left_join(mutate(pivot_wider(as.data.frame(All_Sonde_long),names_from=Parameter,values_from=Value,values_fn = mean),`Date Time`=ymd_hms(`Date Time`)),by=c("Date Time","Ecotope"))  #join sonde data

# Join Continuous Data to WQ and Field Data -------------------------------

WQ_Field_Data_Continuous_data <- WQ_Field_Data %>%
pivot_longer(names_to = "TEST_NAME",values_to="VALUE",6:36)  %>%
mutate(`Date Time`=ISOdate(year(Date),month(Date),day(Date),Hour,Minute,0,tz = "US/Eastern")) %>%
select(-Date,-Hour,-Minute) %>%
bind_rows(mutate(pivot_longer(Continuous_data,names_to = "TEST_NAME",values_to="VALUE",3:19),Position="Mid")) %>%
pivot_wider(names_from =c(TEST_NAME),values_from=VALUE)  

# Save Data ---------------------------------------------------------------

write.csv(WQ_Field_Data_Continuous_data,"./Data/Joined Data/WQ_Field_Data_Continuous_data.csv",row.names = FALSE)
write.csv(Continuous_data,"./Data/Joined Data/Continuous_data.csv",row.names = FALSE)
write.csv(WQ_Field_Data,"./Data/Joined Data/WQ_Field_Data.csv",row.names = FALSE)


 