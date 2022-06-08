rm(list = ls())

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
WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)
WQ_Field_Data <- read_csv("./Data/Joined Data/WQ_Field_Data.csv")

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

Sample_counts <-WQ_Data  %>%
filter(SAMPLE_TYPE=="SAMP") %>%
group_by(TEST_NAME) %>%
summarise(n=n())

# Visualize WQ -------------------------------------------------------------

#All Analyses Concentration over time points and smooth
ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+
scale_x_date(date_breaks="1 month",labels = date_format("%b"))+theme_bw()

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

#DCS Depth vs TP 
ggplot(WQ_Field_Data_Continuous_data,aes(`Average DCS (Field Data)`,`TPO4`,fill=Position))+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.05,0.005),limits=c(0,.05))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .05),alpha=.5,fill="#99d8c9")+ geom_point(shape=21,size=2)+geom_smooth()+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

ggsave(plot = last_plot(),filename="./Figures/Water Depth vs TP.jpeg",width =11, height =8, units = "in")

#DCS Depth vs Temp
ggplot(WQ_Field_Data_Continuous_data,aes(`Average DCS (Field Data)`,`Temp`))+geom_point(shape=21,size=2)+geom_smooth()+theme_bw()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(10,50,5),limits=c(10,50))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)

Temp_DF <-  mutate(WQ_Field_with_continuous_same_rows,`Temp Category`=case_when(is.na(`Temp`)~"No data",
                                                                                `Temp`<20~"Less than 20C",
                                                                                between(`Temp`,20,30)~"20-30C",
                                                                                `Temp`>30~"Greater than 30C"))
#DCS depth vs TP for various temp categories
ggplot(Temp_DF,aes(`Average DCS (Field Data)`,`TPO4`,fill=Ecotope))+
facet_wrap(~`Temp Category`)+scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()


#Depth vs TP for 
Flowing_DF <-  mutate(WQ_Field_with_continuous_same_rows,Flow=case_when(is.na(`Mean inflow (cfs)`)~"No data",
                                                                   `Mean inflow (cfs)`<10~"Less than 10 cfs",
                                                                   `Mean inflow (cfs)`>10~"10+ cfs"))
ggplot(Flowing_DF,aes(`Average DCS (Field Data)`,`TPO4`,fill=Ecotope))+
facet_wrap(~Flow)+scale_y_continuous(breaks=seq(0,.03,0.005),limits=c(0,.03))+geom_rect(aes(xmin = 45.72, ymin = 0, xmax = 76.2, ymax = .03),alpha=.5,fill="#99d8c9")+ 
geom_point(shape=21,size=2)+geom_smooth(fill="grey30",method="lm")+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()



# Wind plots --------------------------------------------------------------

#wind direction and strength on sampling days
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`BELLE GL_WNVS_MPH`,color=`BELLE GL_WNVS_MPH`))+geom_col()+
coord_polar()+scale_color_viridis(option = "D")+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()

#wind direction and strength on all days
ggplot(WQ_Field_Data_Continuous_data,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`BELLE GL_WNVS_MPH`,color=`BELLE GL_WNVS_MPH`))+geom_col()+
coord_polar()+scale_color_viridis(option = "D")+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()

#Wind speed vs TP 
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVS_MPH`,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+#scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Wind speed vs difference in TP 
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVS_MPH`,`Dif TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+#scale_y_continuous(breaks=seq(0,.03,0.005))+
scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#Wind direction vs TP 
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`TPO4`,color=TPO4))+geom_col()+#geom_smooth()+
facet_wrap(~Ecotope)+coord_polar()+
scale_color_viridis(option = "D",direction = -1)+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()

#Wind direction vs difference in TP  
ggplot(WQ_Field_with_continuous_same_rows,aes(`BELLE GL_WNVD_DEGREES CLOCKWI`,`Dif TPO4`,color=abs(`Dif TPO4`)))+geom_col()+#geom_smooth()+
facet_wrap(~Ecotope)+coord_polar()+
scale_color_viridis(option = "D",direction = -1)+
scale_x_continuous(breaks=seq(0,315,45),limits=c(0,360))+theme_bw()


# Flow Figures ------------------------------------------------------------
#inflow
ggplot(WQ_Field_Data_Continuous_data,aes(ymd_hms(`Date Time`),TPO4*100000,fill=Ecotope))+
geom_line(aes(ymd_hms(`Date Time`),`Mean inflow (cfs)`),color="blue")+
geom_line(aes(ymd_hms(`Date Time`),`Mean outflow (cfs)`),color="orange")+ 
geom_point(shape=21,size=2)+ 
geom_path()+
facet_wrap(~Ecotope)+
scale_x_datetime(date_breaks="1 month",labels = date_format("%b %y"),limits = as.POSIXct(c("2021-06-01 00:00:00","2022-03-01 00:00:00")))+coord_cartesian(ylim=c(-800,2000))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#inflow vs TPO4
ggplot(WQ_Field_with_continuous_same_rows,aes(`Mean inflow (cfs)`,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.05,0.005))+
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




# Ash Figures -------------------------------------------------------------

Ash_DF <- WQ_Field_Data %>%
mutate(Ash=if_else(`Date`=="2022-03-01","Yes","No")) %>%
pivot_longer(names_to="TEST_NAME",values_to="VALUE",6:33) 

ggplot(Ash_DF,aes(Ash,`VALUE`,fill=Ash))+geom_boxplot(color="black")+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()
