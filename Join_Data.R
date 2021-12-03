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
All_light_data <- read_csv("Data/HOBO/All_light_data.csv")
All_Sonde_long <- read_csv("Data/Sonde/All_Sonde_long.csv")
WQ_Upstream_Downstream_Tidy <- read_csv("Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")


# Join Data ---------------------------------------------------------------


