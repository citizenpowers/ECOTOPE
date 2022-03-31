library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(viridis)
library(Hmisc)
library(ggpmisc)
library(ggrepel)
library(zoo)
library(readxl)
library(readr)
library(corrplot)
library(GGally)
library(read_excel)

# Import Data -------------------------------------------------------------

WQ_Field_Data_Continuous_data <- read.csv("Data/Joined Data/WQ_Field_Data_Continuous_data.csv",check.names=FALSE)
WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)
PFLUX_All_Combined_Field_Readings <- read_excel("Data/WQ Data/PFLUX All Combined Field Readings.xlsx",sheet="Sheet2")
PFLUX_WQ_LIMSP <- read_excel("Data/WQ Data/PFLUX WQ LIMSP.xlsx")

# Tidy Data ---------------------------------------------------------------

PFLUX_Field_Tidy <- PFLUX_All_Combined_Field_Readings %>%
rowwise() %>%
mutate(`DCS Mean`=(`Depth to Consolidated Substrate 1 (cm)`+`Depth to Consolidated Substrate 2 (cm)`+`Depth to Consolidated Substrate 3 (cm)`)/sum(is.finite(`Depth to Consolidated Substrate 1 (cm)`),is.finite(`Depth to Consolidated Substrate 2 (cm)`),is.finite(`Depth to Consolidated Substrate 3 (cm)`))) %>% 
mutate(`Water Column Mean`=(`Water Column Depth 1 (cm)`+`Water Column Depth 2 (cm)`+`Water Column Depth 3 (cm)`)/sum(is.finite(`Water Column Depth 1 (cm)`),is.finite(`Water Column Depth 2 (cm)`),is.finite(`Water Column Depth 3 (cm)`))) %>%
rename(STATION="Station")%>%
select(STATION,Date,`Water Column Mean`,`DCS Mean`)  
  
PFLUX_WQ_Tidy <- PFLUX_WQ_LIMSP %>%
filter(MATRIX=="SW",SAMPLE_TYPE=="SAMP",TEST_NAME=="TPO4",COLLECT_METHOD=="G",COLLECT_DATE>"2016-01-01",is.na(HABITAT))  %>%
mutate(Date=as.Date(COLLECT_DATE)) %>%
select(Date,STATION,VALUE)%>%
rename(TPO4="VALUE") 

Stations <-filter(count(PFLUX_WQ_Tidy,STATION),n>=20)$STATION  #Create vector of desired stations
Outflow_Stations <- c("ST2C1OUT","ST2C3C200","ST34C3BD11") #Create vector of outflow stations

#Combined WQ and field Data for PFLUX
PFLUX_WQ_Field_Tidy <- PFLUX_WQ_Tidy %>%
left_join(PFLUX_Field_Tidy,by=c("Date","STATION")) %>% 
filter(STATION %in% Stations) #filter to only stations in the Stations vector

#ECOTOPE data with flow data categorized
Flowing_DF <-  WQ_Field_with_continuous_same_rows %>%
mutate(`Inflow Category` =case_when(is.na(`Mean inflow (cfs)`)~"No data",`Mean inflow (cfs)`<10~"Less than 10 cfs",`Mean inflow (cfs)`>10~"10+ cfs")) %>%
mutate(`Outflow Category` =case_when(is.na(`Mean outflow (cfs)`)~"No data",`Mean outflow (cfs)`<10~"Less than 10 cfs",between(`Mean outflow (cfs)`,10,200)~"10-200 cfs",`Mean outflow (cfs)`>200~"200+ cfs")) 

ggplot(Flowing_DF,aes(`Mean outflow (cfs)`))+geom_histogram()

test <- as.data.frame(cut2(Flowing_DF$`Mean outflow (cfs)`,g=4))
# Water Depth Figures -----------------------------------------------------------------

#Water depth vs TP for ECOTOPE sites when inflow is flowing and stagnant
ggplot(Flowing_DF,aes(`Average DCS (Field Data)`,`TPO4`,fill=Ecotope))+
facet_wrap(~`Inflow Category`)+scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Water depth vs TP for ECOTOPE sites when outflow is flowing and stagnant
ggplot(Flowing_DF,aes(`Average DCS (Field Data)`,`TPO4`,fill=Ecotope))+
facet_wrap(~`Outflow Category`)+scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Water depth vs TP PFLUX sites
ggplot(PFLUX_WQ_Field_Tidy,aes(`DCS Mean`,`TPO4`))+
facet_wrap(~STATION)+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .3),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DCS vs TP at outflow PFLUX sites
ggplot(filter(PFLUX_WQ_Field_Tidy,STATION %in% Outflow_Stations),aes(`DCS Mean`,`TPO4`))+
facet_wrap(~STATION)+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+
geom_point(shape=21,size=2)+geom_smooth(fill="grey30")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Water depth vs TP at outflow PFLUX sites (Not enough sites)
ggplot(filter(PFLUX_WQ_Field_Tidy,STATION %in% Outflow_Stations),aes(`Water Column Mean`,`TPO4`))+
facet_wrap(~STATION)+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


# Flow Figures ------------------------------------------------------------

#Outflow vs TP for ecotope sites
ggplot(Flowing_DF,aes(`Mean outflow (cfs)`,`TPO4`,fill=Ecotope))+
scale_y_continuous(breaks=seq(0,.05,0.005))+
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Outflow vs TP for ecotope sites (stagnant conditions removed)
ggplot(filter(Flowing_DF,`Mean outflow (cfs)`>1),aes(`Mean outflow (cfs)`,`TPO4`,fill=Ecotope))+
scale_y_continuous(breaks=seq(0,.05,0.005))+
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Inflow vs TP for ecotope sites
ggplot(Flowing_DF,aes(`Mean inflow (cfs)`*-1,`TPO4`,fill=Ecotope))+
scale_y_continuous(breaks=seq(0,.05,0.005))+
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


# Wind figures ------------------------------------------------------------

#Outflow vs TP for ecotope sites
ggplot(Flowing_DF,aes(`BELLE GL_WNVS_MPH`,`TPO4`,fill=Ecotope))+
scale_y_continuous(breaks=seq(0,.05,0.005))+
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


