#THe goal of this script is to track water depth and flow when sampling. 
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

WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)



# Tidy Data ---------------------------------------------------------------

Flow_Depth_TP <- WQ_Field_with_continuous_same_rows %>%
select( Date,Ecotope,Hour,Minute,`DCS (Field Data)`,`Mean outflow (cfs)`) %>%
group_by(Date,Ecotope) %>%
summarise(`DCS Mean`=mean(`DCS (Field Data)`,na.rm=TRUE),`Outflow Mean`=mean(`Mean outflow (cfs)`,na.rm=TRUE))


# Figures -----------------------------------------------------------------

#flow vs water depth for sampling dates 
ggplot(Flow_Depth_TP,aes(`Outflow Mean`,`DCS Mean`,fill=as.factor(Date)))+geom_point(shape=21,size=3,color="black")+theme_bw()+ geom_vline(aes(xintercept = c(300)))+ geom_hline(yintercept = c(45,76))+
scale_y_continuous(breaks=seq(0,130,10),limits=c(0,130))

ggsave(plot = last_plot(),filename="./Figures/Sample Collection Matrix.jpeg",width =11, height =8 , units = "in")
