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



# Import data -------------------------------------------------------------

#Deployment 1
Naiad_Mid_Light_20210609_Data <- select(rename(mutate(read_csv("Data/HOBO/20210609_Naiad_Mid.csv", skip = 2),Site="Naiad",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Naiad_Top_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Naiad_Top.csv", skip = 2),Site="Naiad",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Mid_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Bare_Mid.csv", skip = 2),Site="Bare",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Top_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/200609_Bare_Top.csv", skip = 2),Site="Bare",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_Mid_Light_20210609_Data <- select(rename(mutate( read_csv("Data/HOBO/20210609_Chara_Mid.csv", skip = 2),Site="Chara",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_Top_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Chara_Top.csv", skip = 2),Site="Chara",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Mid_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Mixed_Mid.csv", skip = 2),Site="Mixed",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Top_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Mixed_Top.csv", skip = 2),Site="Mixed",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Typha_Mid_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Cat_Mid.csv", skip = 2),Site="Typha",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Typha_Top_Light_20210609_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210609_Cat_top.csv", skip = 2),Site="Typha",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)

#Deployment 2
Naiad_Mid_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Naiad_Mid_Hobo.csv", skip = 2),Site="Naiad",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Naiad_Top_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Naiad_Top_Hobo.csv", skip = 2),Site="Naiad",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Mid_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Bare_Mid_Hobo.csv", skip = 2),Site="Bare",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Top_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Bare_Top_Hobo.csv", skip = 2),Site="Bare",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_Mid_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Chara_Mid_Hobo.csv", skip = 2),Site="Chara",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_Top_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Chara_Top_Hobo.csv", skip = 2),Site="Chara",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Mid_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Mixed_Mid_Hobo.csv", skip = 2),Site="Mixed",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Top_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Mixed_Top_Hobo.csv", skip = 2),Site="Mixed",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Typha_Mid_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Cat_Mid_Hobo.csv", skip = 2),Site="Typha",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Typha_Top_Light_20210817_Data <-  select(rename(mutate(read_csv("Data/HOBO/20210817_Cat_top_Hobo.csv", skip = 2),Site="Typha",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)

#Deployment 3
Naiad_Mid_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Naiad_Mid.csv", skip = 2),Site="Naiad",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,10:11)
Naiad_Top_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Naiad_Top.csv", skip = 2),Site="Naiad",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Mid_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Bare_Mid.csv", skip = 2),Site="Bare",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Top_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Bare_Top.csv", skip = 2),Site="Bare",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_Mid_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Chara_Mid.csv", skip = 2),Site="Chara",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_Top_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Chara_top.csv", skip = 2),Site="Chara",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Mid_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Mixed_Mid.csv", skip = 2),Site="Mixed",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Top_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Mixed_Top.csv", skip = 2),Site="Mixed",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,6:7)
Typha_Mid_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Cat_Mid.csv", skip = 2),Site="Typha",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Typha_Top_Light_20211123_Data <-  select(rename(mutate(read_csv("Data/HOBO/20211123_Cat_top.csv", skip = 2),Site="Typha",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)

#Deployment 4 
Cat_top_light_20220503_Data <-select(rename(mutate(read_csv("Data/HOBO/20220503_Cat_top.csv", skip = 1),Site="Typha",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Cat_mid_light_20220503_Data <-select(rename(mutate(read_csv("Data/HOBO/20220503_Cat_Mid.csv", skip = 1),Site="Typha",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_top_light_20220503_Data <-select(rename(mutate(read_csv("Data/HOBO/20220503_Chara_Top.csv", skip = 1),Site="Chara",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_mid_light_20220503_Data <-select(rename(mutate(read_csv("Data/HOBO/20220503_Chara_Mid.csv", skip = 1),Site="Chara",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Mid_Light_20220503_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220503_Mixed_Mid.csv", skip = 2),Site="Mixed",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Top_Light_20220503_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220503_Mixed_Top.csv", skip = 2),Site="Mixed",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,6:7)
Naiad_Mid_Light_20220503_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220503_Naiad_Mid.csv", skip = 2),Site="Naiad",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,10:11)
Naiad_Top_Light_20220503_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220503_Naiad_Top.csv", skip = 2),Site="Naiad",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Mid_Light_20220503_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220503_Bare_Mid.csv", skip = 2),Site="Bare",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,6:7)  #data corrupt? 
Bare_Top_Light_20220503_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220503_Bare_Top.csv", skip = 2),Site="Bare",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,10:11)

#Deployment 5 
Cat_top_light_20220531_Data <-select(rename(mutate(read_csv("Data/HOBO/20220531_Cat_top.csv", skip = 1),Site="Typha",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Cat_mid_light_20220531_Data <-select(rename(mutate(read_csv("Data/HOBO/20220531_Cat_Mid.csv", skip = 1),Site="Typha",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_top_light_20220531_Data <-select(rename(mutate(read_csv("Data/HOBO/20220531_Chara_Top.csv", skip = 1),Site="Chara",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Chara_mid_light_20220531_Data <-select(rename(mutate(read_csv("Data/HOBO/20220531_Chara_Mid.csv", skip = 1),Site="Chara",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Mid_Light_20220531_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220531_Mix_mid.csv", skip = 2),Site="Mixed",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Mixed_Top_Light_20220531_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220531_Mix_top.csv", skip = 2),Site="Mixed",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Naiad_Mid_Light_20220531_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220531_Naiad_Mid.csv", skip = 2),Site="Naiad",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Naiad_Top_Light_20220531_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220531_Naiad_Top.csv", skip = 1),Site="Naiad",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)
Bare_Mid_Light_20220531_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220531_Bare_Mid.csv", skip = 2),Site="Bare",Position="Mid"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)  
Bare_Top_Light_20220531_Data <-  select(rename(mutate(read_csv("Data/HOBO/20220531_Bare_Top.csv", skip = 2),Site="Bare",Position="Top"),`Date Time`=2,`Temp C°`=3,`Light Intensity Lux`=4),2:4,9:10)


# Tidy -----------------------------------------------------------

#Deployment 1
Light_Data_060921 <- bind_rows(Naiad_Mid_Light_20210609_Data,Naiad_Top_Light_20210609_Data,Bare_Mid_Light_20210609_Data,Bare_Top_Light_20210609_Data,Chara_Mid_Light_20210609_Data,Chara_Top_Light_20210609_Data,Mixed_Mid_Light_20210609_Data,Mixed_Top_Light_20210609_Data,Typha_Mid_Light_20210609_Data,Typha_Top_Light_20210609_Data  ) %>%
mutate(`Date Time`=mdy_hms(`Date Time`,tz="America/New_York")) %>%
filter(`Date Time`>"2021-06-09 00:30:00",`Date Time`<"2021-06-29 23:30:00")

#Deployment 2
Light_Data_081721 <- rbind(Naiad_Mid_Light_20210817_Data,Naiad_Top_Light_20210817_Data,Bare_Mid_Light_20210817_Data,Bare_Top_Light_20210817_Data,Chara_Mid_Light_20210817_Data,Chara_Top_Light_20210817_Data,Mixed_Mid_Light_20210817_Data,Mixed_Top_Light_20210817_Data,Typha_Mid_Light_20210817_Data,Typha_Top_Light_20210817_Data  ) %>%
mutate(`Date Time`=mdy_hms(`Date Time`,tz="America/New_York"))  %>%
filter(`Date Time`>"2021-08-18 00:00:00",`Date Time`<"2021-11-09 00:00:00")

#Deployment 3
Light_Data_20211123 <- rbind(Naiad_Mid_Light_20211123_Data,Naiad_Top_Light_20211123_Data,Bare_Mid_Light_20211123_Data,Bare_Top_Light_20211123_Data,Chara_Mid_Light_20211123_Data,Chara_Top_Light_20211123_Data,Mixed_Mid_Light_20211123_Data,Mixed_Top_Light_20211123_Data,Typha_Mid_Light_20211123_Data,Typha_Top_Light_20211123_Data  ) %>%
mutate(`Date Time`=mdy_hms(`Date Time`,tz="America/New_York"))  %>%
filter(`Date Time`>"2021-11-23 00:00:00",`Date Time`<"2022-01-11 10:00:00")

#Deployment 4 
Light_Data_20220503 <- rbind(Mixed_Top_Light_20220503_Data,Mixed_Mid_Light_20220503_Data,Bare_Top_Light_20220503_Data,Naiad_Top_Light_20220503_Data,Naiad_Mid_Light_20220503_Data,Chara_mid_light_20220503_Data,Chara_top_light_20220503_Data,Cat_mid_light_20220503_Data,Cat_top_light_20220503_Data) %>%
mutate(`Date Time`=mdy_hms(`Date Time`,tz="America/New_York"))  %>%  
filter(minute(`Date Time`)%%30==0) %>%  #Mixed_Top_Light_20220503_Data collected at 1 minute frequency. Data filtered to 30 min frequency
filter(is.POSIXct(`Date Time`)) %>%
filter(`Date Time`<"2022-04-26 00:00:00")

#Deployment 5 
Light_Data_20220531 <- rbind(Mixed_Top_Light_20220531_Data,Mixed_Mid_Light_20220531_Data,Bare_Top_Light_20220531_Data,Naiad_Top_Light_20220531_Data,Naiad_Mid_Light_20220531_Data,Chara_mid_light_20220531_Data,Chara_top_light_20220531_Data,Cat_mid_light_20220531_Data,Cat_top_light_20220531_Data) %>%
mutate(`Date Time`=mdy_hms(`Date Time`,tz="America/New_York"))  %>%  
filter(is.POSIXct(`Date Time`)) %>%
filter(`Date Time`>"2022-05-12 12:00:00",`Date Time`<"2022-05-31 10:00:00")


# Join Data ---------------------------------------------------------------

All_light_data <- bind_rows(Light_Data_060921,Light_Data_081721,Light_Data_20211123,Light_Data_20220503,Light_Data_20220531) %>% rename(`Ecotope`="Site") 

# Analyze data ------------------------------------------------------------

All_light_data_by_hour <-All_light_data %>%
mutate(Hour=hour(`Date Time`),Date=as.Date(`Date Time`)) %>%
group_by(Date,Ecotope,Position,Hour) %>%
summarise(`Hourly Light Intensity`=mean(`Light Intensity Lux`,na.rm = TRUE))  
  

All_light_data_by_hour_table <-All_light_data %>%
mutate(Hour=hour(`Date Time`),Date=as.Date(`Date Time`)) %>%
group_by(Ecotope,Position,Hour) %>%
summarise(`Hourly Light Intensity`=mean(`Light Intensity Lux`,na.rm = TRUE))  


# Save Data ---------------------------------------------------------------

write.csv(select(All_light_data,-`Temp C°`) ,"./Data/HOBO/All_light_data.csv",row.names = FALSE) 

# Figures -----------------------------------------------------------------
ggplot(All_light_data,aes(`Date Time`,`Light Intensity Lux`,color=Position,fill=Position))+geom_point(shape=21,alpha=.5)+
facet_wrap(~Ecotope,scales = "free_y")+  scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(All_light_data_by_hour,aes(as.factor(Hour),`Hourly Light Intensity`,color=Position,fill=Position))+geom_boxplot()+
facet_wrap(~Ecotope)+  scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(All_light_data_by_hour_table,aes(as.factor(Hour),`Hourly Light Intensity`,color=Position,fill=Position))+geom_col(position = "dodge")+
facet_wrap(~Ecotope)+  scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()
