rm(list = ls())

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
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


# Import Data -------------------------------------------------------------

BELLW_WNVS_BK <- get_hydro(dbkey = "KV270", date_min="2021-06-01",date_max=as.character(today()))  #max daily wind speed in at belleglade weather station
BELLW_WNVD_BK <- get_hydro(dbkey = "IW852", date_min="2021-06-01",date_max=as.character(today()))  #max daily wind direction in at belleglade weather station


# Tidy Data ---------------------------------------------------------------

Wind_data <-  rename(BELLW_WNVS_BK,`Date Time`="date" ) %>%
left_join(rename(BELLW_WNVD_BK,`Date Time`="date" ),by="Date Time") 



# Save Data ---------------------------------------------------------------

write.csv(Wind_data ,"./Data/Weather Data/Wind_data.csv",row.names = FALSE)

