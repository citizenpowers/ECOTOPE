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
library(devtools)
library(artyfarty)
library(ggpomological)
library(ggthemr)
library(interp)
# Import Data -------------------------------------------------------------


WQ_Upstream_Downstream_Tidy <- read_csv("./Data/WQ Data/WQ_Upstream_Downstream_Tidy.csv")
WQ_Data_Tidy <- read_csv("./Data/WQ Data/WQ_Data_Tidy.csv")
WQ_Data <- read_excel("Data/WQ Data/WQ Data.xlsx",sheet = "Sheet1")
WQ_Field_Data_Continuous_data <- read.csv("Data/Joined Data/WQ_Field_Data_Continuous_data.csv",check.names=FALSE)
WQ_Field_with_continuous_same_rows <- read.csv("./Data/Joined Data/WQ_Field_with_continuous_same_rows.csv",check.names=FALSE)
WQ_Field_Data <- read_csv("./Data/Joined Data/WQ_Field_Data.csv")
TP_Budget <- read_csv("./Data/P Budget/TP_Budget.csv")

# Theme -------------------------------------------------------------------

ggthemr("flat dark",type="outer", layout="scientific")

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
all_analytes_plot <-ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Date,`VALUE`,color=Ecotope,fill=Ecotope))+geom_point()+geom_smooth(se=FALSE)+
facet_wrap(~TEST_NAME,scales = "free_y")+
#scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+
#scale_color_pomological()+ scale_fill_pomological()  +theme_pomological_fancy()+  #pomological theme
#scale_fill_manual(values = pal("ft"))+scale_color_manual(values = pal("ft"))+ theme_ft()+ #sparkly stones theme
scale_x_date(date_breaks="3 month",labels = date_format("%b %y"))+ ylab(" ")+guides(x =  guide_axis(angle = 40))

ggsave(plot = last_plot(),filename="./Figures/All WQ Analytes over time.jpeg",width =13.333, height =7.5, units = "in")

#TPO4 Concentration over time points and smooth
TPO4_plot <-ggplot(WQ_Field_Data,aes(Date,TPO4*1000,color=Ecotope,fill=Ecotope,linetype=Ecotope))+
geom_point(aes(Date,TPO4*1000,color=Ecotope,fill=Ecotope,shape=Position),size=2)+
geom_smooth(se=FALSE)+
#scale_fill_manual(values = pal("ft"))+scale_color_manual(values = pal("ft"))+theme_ft()+
scale_shape_manual(values = c(21:24)) + 
scale_x_date(date_breaks="1 month",labels = date_format("%b %y"))+guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

ggthemr("flat dark",type="outer", layout="scientific")
TPO4_plot

ggsave(plot = last_plot(),filename="./Figures/TPO4 over time-flat dark.jpeg",width =13.333, height =7.5, units = "in")

#TPO4 boxplots
TPO4_boxplot <-ggplot(filter(WQ_Field_Data,Position=="Downstream"),aes(Ecotope,TPO4*1000,color=Ecotope))+
geom_boxplot(fill="#1F77B4")+ scale_y_continuous(breaks=seq(0,40,5),limits=c(0,40))+ guides(x =  guide_axis(angle = 40))+labs(y=expression(TP~(mu~g~L^-1)))

ggthemr("flat dark",type="outer", layout="scientific")
TPO4_boxplot

ggsave(plot = last_plot(),filename="./Figures/TPO4 Boxplot-flat dark.jpeg",width =8, height =6, units = "in")


#All Analytes Concentration boxplots
ggplot(pivot_longer(WQ_Field_Data,names_to="TEST_NAME",values_to="VALUE",6:33),aes(Ecotope,`VALUE`,fill=Ecotope))+geom_boxplot(color="black")+
facet_wrap(~TEST_NAME,scales = "free_y")+scale_fill_brewer(palette = "Set2",direction = -1)+theme_bw()+scale_color_brewer(palette = "Set2",direction = -1)

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


# Water depth vs TP -----------------------------------------------------

#Depth over time with TP differences
ggplot(select(WQ_Field_Data_Continuous_data,`Average DCS (Field Data)`,`DCS Levelogger`,`Date Time`,Position,Ecotope,`Dif TPO4`),aes(ymd_hms(`Date Time`),`Average DCS (Field Data)`,fill=Position))+geom_point(shape=21,size=2)+
geom_line(aes(ymd_hms(`Date Time`),`DCS Levelogger`*100))+
geom_line(aes(ymd_hms(`Date Time`),`Dif TPO4`*10000),color="red")+ 
facet_wrap(~Ecotope)+
scale_x_datetime(date_breaks="1 month",labels = date_format("%b %y"),limits = as.POSIXct(c("2021-06-01 00:00:00","2022-05-31 00:00:00")))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#DCS Depth vs TP 
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream"),aes(`DCS (Field Data)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,50,5))+
geom_rect(aes(xmin = 45.72, ymin = -Inf, xmax = 76.2, ymax = 40),alpha=.5,fill="#787C99",color="#787C99")+geom_point(shape=21,size=2,color="grey70")+geom_smooth()+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Depth to Consolidated Substrate (cm)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Water Depth.jpeg",width =5, height =6, units = "in")


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


# Flow vs TP  ------------------------------------------------------------
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
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream"),aes(`Mean inflow (cfs)`,`TPO4`))+geom_point(shape=21,size=2)+geom_smooth()+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,.05,0.005))+
scale_fill_brewer(palette = "Set2",direction = -1)+scale_color_brewer(palette = "Set2",direction = -1)+theme_bw()

#outflow vs TPO4
ggplot(filter(WQ_Field_with_continuous_same_rows,Position=="Downstream"),aes(`Mean outflow (cfs)`,`TPO4`*1000,fill=Ecotope,color=Ecotope))+geom_smooth()+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,50,5))+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Outflow (cfs)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Outflow.jpeg",width =5, height =6, units = "in")


# Water Depth vs Flow and TP ----------------------------------------------
#Remove NAs
contour_data <- WQ_Field_with_continuous_same_rows %>%
drop_na(TPO4) %>%
drop_na(`DCS (Field Data)`) %>%
drop_na(`Mean outflow (cfs)`) 

#Interpolate to grid form
contour_grid <- with(contour_data, interp::interp(`DCS (Field Data)`, `Mean outflow (cfs)`, TPO4,duplicate="mean"))
griddf <- subset(data.frame(`Depth` = rep(contour_grid$x, nrow(contour_grid$z)),
                            `CFS`= rep(contour_grid$y, each = ncol(contour_grid$z)),
                            z = as.numeric(contour_grid$z)),!is.na(z)) %>%
rename(`DCS (Field Data)`="Depth",`Mean outflow (cfs)`="CFS",TPO4="z")


TP_Contour_Plot <-ggplot(griddf, aes(`DCS (Field Data)`,`Mean outflow (cfs)` , z = TPO4*1000)) +
geom_contour_filled() +  xlab("Depth to Consolidated Substrate (cm)")+ylab("Outflow (cfs)")+
geom_point(data = contour_data, aes(`DCS (Field Data)`,`Mean outflow (cfs)`))+guides(fill=guide_legend(title=expression(TP~(mu~g~L^-1))))

ggthemr("flat dark",type="outer", layout="scientific")
TP_Contour_Plot

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Flow vs Depth.jpeg",width =13.333, height =7.5, units = "in")

# TP vs physico-chemical parameters ----------------------------------------
ggplot(filter(WQ_Field_Data_Continuous_data,Position=="Downstream"),aes(`Temp`,`TPO4`*1000,color=Ecotope,fill=Ecotope))+geom_smooth()+geom_point(shape=21,size=2,color="grey70")+
facet_wrap(~Ecotope)+scale_y_continuous(breaks=seq(0,50,5))+
ylab(expression(TP~(mu~g~L^-1)))+xlab("Temp (C)")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 vs Temp.jpeg",width =5, height =6, units = "in")


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




# TP Budget Figures -------------------------------------------------------

Cumulative_TP <- TP_Budget %>%
pivot_longer(25:31,names_to="Ecotope",values_to="Value") %>%
filter(row_number() %% 4==1) #Select every 4th row 

#All Analyses Concentration over time points and smooth
FWM_TP_PLot <- ggplot(Cumulative_TP,aes(`Date Time`,`Value`,color=Ecotope))+geom_point()+
scale_x_datetime(date_breaks="3 month",labels = date_format("%b %y"))+ ylab("P (kg)")+guides(x =  guide_axis(angle = 40),position="bottom")


ggthemr("flat dark",type="outer", layout="scientific")
FWM_TP_PLot

ggsave(plot = last_plot(),filename="./Figures/Cumulative TP.jpeg",width =7, height =5, units = "in")











# TP Differences ----------------------------------------------------------
#Table with summary stats
TP_Diff_Summary_Stats <- filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4") %>%
group_by(Ecotope) %>%  
summarise(n(),`Median diff`=median(Difference,na.rm=TRUE),`mean diff`=mean(Difference,na.rm=TRUE))
  
#TPO4 Differences
ggplot(filter(WQ_Upstream_Downstream_Tidy,TEST_NAME=="TPO4"),aes(Ecotope,`Difference`*1000,fill=Ecotope))+
scale_y_continuous(breaks=seq(-50,50,5))+ geom_hline(aes(yintercept = 0),linetype="dashed",color="white")+geom_boxplot()+
ylab(expression(Upstream-Downstream~TP~(mu~g~L^-1)))+xlab("Ecotope")+guides(fill="none",color="none")

ggsave(plot = last_plot(),filename="./Figures/TPO4 Differences Upstream-Downstream.jpeg",width =8, height =6, units = "in")

