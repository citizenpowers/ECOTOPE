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
All_light_data <- read.csv("Data/HOBO/All_light_data.csv")
All_Sonde_long <- read_csv("Data/Sonde/All_Sonde_long.csv")
WQ_Upstream_Downstream_Tidy <- read_csv("Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv") # Water quality with diffrences between upstream and downstream calculated
WQ_Data_Tidy <- read_csv("Data/WQ Data/WQ_Data_Tidy.csv")  #all WQ data 
Field_data <- read_csv("Data/Field Data/Field_data.csv")


# WQ and Field data---------------------------------------------------------------

WQ_Field_Data <- WQ_Data_Tidy %>% 
filter(MATRIX=="SW",COLLECT_METHOD=="G") %>%
select(Date,Ecotope,Position,TEST_NAME,VALUE)  %>%
bind_rows(Field_data) %>%
pivot_wider(names_from =c(TEST_NAME),values_from=VALUE,values_fn = mean) #Used values_fn = mean to average duplicate values


# Join WQ and Field Data to continuous data -------------------------------

All_data <-  setNames(as.data.frame(seq(from=ISOdate(2021,6,01,0,0,0,tz = "US/Eastern"), to=ISOdate(2022,05,31,0,0,0,tz = "US/Eastern"),by = "30 min")),"date") %>%
 
pivot_wider(All_light_data,names_from=Position,values_from=Light.Intensity.Lux)
