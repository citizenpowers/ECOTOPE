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

# Import Data -------------------------------------------------------------


WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Field_Data_Continuous_data <- read.csv("Data/Joined Data/WQ_Field_Data_Continuous_data.csv",check.names=FALSE)

# QC Blank Evaluation -----------------------------------------------------

QC_Blanks_Tidy <-WQ_Data  %>%
filter(SAMPLE_TYPE=="FCEB") %>%
group_by(TEST_NAME,REMARK_CODE) %>%
summarise(n=n())


# Contamination Evaluation ------------------------------------------------

Sample_Remarks_Code_Tidy <-WQ_Data  %>%
filter(SAMPLE_TYPE=="SAMP") %>%
group_by(TEST_NAME,REMARK_CODE) %>%
summarise(n=n())



# Visualize WQ -------------------------------------------------------------

#All Analyses Concentration over time points and smooth
ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All Analytes Concentration boxplots
ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Ecotope,`VALUE`,fill=Ecotope))+geom_boxplot(color="black")+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All Analytes Differences (Up-Down)- points and smooth 
ggplot(pivot_longer(WQ_Field_Diff_Data,names_to="TEST_NAME",values_to="VALUE",34:61),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- columns
ggplot(filter(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),TEST_NAME=="TPO4"),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_col(position = "dodge")+#geom_line(size=1)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#TPO4 only Differences (Up-Down)- points and smooth
ggplot(filter(pivot_longer(WQ_Field_Diff_Data,names_to="TEST_NAME",values_to="VALUE",34:61),TEST_NAME=="Dif TPO4"),aes(Date,VALUE,color=Ecotope,fill=Ecotope))+geom_smooth(size=1,se=FALSE)+geom_point()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down)- Box plots
ggplot(pivot_longer(WQ_Field_Diff_Data,names_to="TEST_NAME",values_to="VALUE",34:61),aes(Ecotope,VALUE,fill=Ecotope))+geom_boxplot()+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#All analytes Differences (Up-Down) by magnitude of average (Up+dowwn)/2- Points and smooth
ggplot(WQ_Upstream_Downstream_Tidy,aes((`Upstream Values`+`Downstream Values`)/2,`Difference`,fill=Ecotope,color=Ecotope))+geom_point(shape=21)+geom_smooth(se=FALSE)+geom_hline(yintercept=0)+
facet_wrap(~TEST_NAME,scales = "free")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


# Water depth figures -----------------------------------------------------

#Depth over time with TP differences
ggplot(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`,`Date Time`,Position,Ecotope,`Dif TPO4`),aes(ymd_hms(`Date Time`),`Average DCS (Field Data)`,fill=Position))+geom_point(shape=21,size=2)+
geom_line(aes(ymd_hms(`Date Time`),`DCS Levelogger`*100))+
geom_line(aes(ymd_hms(`Date Time`),`Dif TPO4`*10000),color="red")+ 
facet_wrap(~Ecotope)+
scale_x_datetime(date_breaks="1 month",labels = date_format("%b %y"),limits = as.POSIXct(c("2021-06-01 00:00:00","2022-03-01 00:00:00")))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Depth vs TP Differences
ggplot(WQ_Field_Data_Continuous_data,aes(`Average DCS (Field Data)`,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


# TP vs physico-chemical parameters ----------------------------------------
ggplot(WQ_Field_Data_Continuous_data,aes(`Temp`,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(SpCond,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(pH,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(DO,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+scale_x_continuous(limits=c(0,25))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

# TP Differences vs physico-chemical parameters ----------------------------------------
ggplot(WQ_Field_Data_Continuous_data,aes(`Temp`,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(SpCond,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(pH,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggplot(WQ_Field_Data_Continuous_data,aes(DO,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.03,0.005))+scale_x_continuous(limits=c(0,25))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


