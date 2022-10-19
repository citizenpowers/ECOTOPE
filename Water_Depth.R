rm(list = ls())

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


# Import data -------------------------------------------------------------

#Deployment 1
Bare_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609  Bare.csv",  skip = 11),Site="Bare")
Typha_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Typha.csv",  skip = 11),Site="Typha")
Mixed_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Mixed.csv",  skip = 11),Site="Mixed")
Chara_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Chara.csv",  skip = 11),Site="Chara")
Naiad_Depth_20210609_Data <- mutate(read_csv("Data/Levelogger/20210609 Naiad.csv",  skip = 11),Site="Southern Naiad")

#Deployment 2
Bare_Depth_20211123_Data <- mutate(read_csv("Data/Levelogger/20211123_Bare.csv",  skip = 11),Site="Bare")
Typha_Depth_20211123_Data <- mutate(read_csv("Data/Levelogger/20211123_Typha.csv",  skip = 11),Site="Typha")
Mixed_Depth_20211123_Data <- mutate(read_csv("Data/Levelogger/20211123_Mixed.csv",  skip = 11),Site="Mixed")
Chara_Depth_20211123_Data <- mutate(read_csv("Data/Levelogger/20211123_Chara.csv",  skip = 11),Site="Chara")
Naiad_Depth_20211123_Data <- mutate(read_csv("Data/Levelogger/20211123_Naiad.csv",  skip = 11),Site="Southern Naiad")

#Deployment 3
Bare_Depth_20220531_Data <- mutate(read_csv("Data/Levelogger/20220531_Bare.csv",  skip = 11),Site="Bare")
Typha_Depth_20220531_Data <- mutate(read_csv("Data/Levelogger/20220531_Typha.csv",  skip = 11),Site="Typha")
Mixed_Depth_20220531_Data <- mutate(read_csv("Data/Levelogger/20220531_Mix.csv",  skip = 11),Site="Mixed")
Chara_Depth_20220531_Data <- mutate(read_csv("Data/Levelogger/20220531_Chara.csv",  skip = 11),Site="Chara")
Naiad_Depth_20220531_Data <- mutate(read_csv("Data/Levelogger/20220531_Naiad.csv",  skip = 11),Site="Southern Naiad") 

#Import Stage Data
Inflow_Stage_BK <- get_hydro(dbkey = "T9942", date_min="2021-06-08",date_max=as.character(today()))  #DBHYDRO data for inflow to Cell 2B of STA34
Outflow_Stage_BK <-get_hydro(dbkey = "T1049", date_min="2021-06-08",date_max=as.character(today()))  #DBHYDRO data for outflow of cell 2B


# Tidy Data ---------------------------------------------------------------
#Deployment 1
Water_Depth_20210609_Data <- bind_rows(Bare_Depth_20210609_Data,Typha_Depth_20210609_Data,Mixed_Depth_20210609_Data ,Chara_Depth_20210609_Data,Naiad_Depth_20210609_Data) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," "),tz="America/New_York"))  %>%
filter(`Date Time`>"2021-06-09 11:30:00") %>%
filter(`Date Time`<"2021-11-09 10:00:00")  

#Deployment 2
Water_Depth_20211123_Data <- bind_rows(Bare_Depth_20211123_Data,Typha_Depth_20211123_Data,Mixed_Depth_20211123_Data ,Chara_Depth_20211123_Data,Naiad_Depth_20211123_Data) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," "),tz="America/New_York"))  %>%
filter(`Date Time`>"2021-11-23 12:00:00") %>%
filter(`Date Time`<"2022-01-11 10:30:00")  

#Deployment 3
Water_Depth_20220531_Data <- bind_rows(Bare_Depth_20220531_Data,Typha_Depth_20220531_Data,Mixed_Depth_20220531_Data ,Chara_Depth_20220531_Data,Naiad_Depth_20220531_Data) %>%
mutate(`Date Time`=mdy_hms(paste(Date," ",Time," "),tz="America/New_York"))  %>%
mutate(`Date Time`= case_when((Site=="Southern Naiad" & (second(`Date Time`)==39)== 1) ~ `Date Time`+seconds(21),
                              (Site=="Southern Naiad" & (second(`Date Time`)==59)== 1) ~ `Date Time`+seconds(1),
                              is.POSIXct(`Date Time`) ~ `Date Time`)) %>%
filter(ifelse(Site=="Chara" & `Date Time`<= "2022-03-01 13:00:00",FALSE,TRUE)) %>%
filter(`Date Time`>"2022-02-01 12:00:00") %>%
filter(`Date Time`<"2022-05-31 09:00:00")  

Inflow_outflow_data <-  setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(year(today()),month(today()),day(today()),0,0,0,tz = "US/Eastern"),by = "min")),"date") %>%
left_join(Inflow_Stage_BK ,by="date") %>%  
left_join(Outflow_Stage_BK,by="date") %>%
fill(`G378C_T_STG_ft NGVD29`,`G379B_H_STG_ft NGVD29`) %>%
mutate(`Inflow Est. Water Depth`=(`G378C_T_STG_ft NGVD29`-9.4)/3.28084,`Outflow Est. Water Depth`=(`G379B_H_STG_ft NGVD29`-9.4)/3.28084)  %>%
select(date,`Inflow Est. Water Depth`,`Outflow Est. Water Depth`)  %>%
pivot_longer(names_to = "Site",values_to="level",2:3)  %>%
rename(`Date Time`="date") %>%
filter(if_else(minute(`Date Time`) %in% c(0,30),TRUE,FALSE)) 

#join Data
Water_Depth_Data <- bind_rows(Water_Depth_20210609_Data,Water_Depth_20211123_Data ,Water_Depth_20220531_Data) %>%
bind_rows(Inflow_outflow_data) %>%
select(`Date Time`,level,Site) %>%  
pivot_wider(names_from = "Site", values_from="level") %>%
pivot_longer(names_to = "Site",values_to="level",2:8) 
# Save data ---------------------------------------------------------------

write.csv(Water_Depth_Data,"./Data/Levelogger/Water_Depth_Data.csv",row.names = FALSE)  

# Figures -----------------------------------------------------------------
#Water Depth over Time
ggplot(Water_Depth_Data,aes(`Date Time`,level*100,color=Site,fill=Site))+geom_line(size=1)+
scale_y_continuous(breaks = pretty_breaks(n=10))+scale_x_datetime(date_breaks="1 month",labels = date_format("%b"))+coord_cartesian(xlim=as.POSIXct(c("2021-06-09 12:00:00","2022-05-31 12:00:00")))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()+
labs(title="Water Depth at Sites",y="Water Depth (cm)",x="Date")

#Water Depth 1 week
ggplot(Water_Depth_Data,aes(`Date Time`,level*100,color=Site,fill=Site))+geom_line(size=1)+
scale_y_continuous(breaks = pretty_breaks(n=10))+scale_x_datetime(date_breaks="1 day",labels = date_format("%b %d"),limits = as.POSIXct(c("2021-09-15 00:00:00","2021-09-22 00:00:00")))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()+
labs(title="Water Depth at Sites",y="Water Depth (cm)",x="Date")






